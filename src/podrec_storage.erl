-module(podrec_storage).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

% actions:
%  - get file ABC?
%  - for a given file:
%    - last time fetched?
%    - last time requested?
%    - modification time (as given by the original url)
%    - orig_url
%  - put new version of file ABC, possibly
%  - delete file
%
%  - gen_server
%  - file paths are only handled internally, to avoid race conditions
%    - get_file() returns a file descriptor

-behaviour(gen_server).

%% API
-export([start_link/1,
         get_file_rev/2,
         is_cached_file_recent/2,
         exists_cached_file/1,
         create_temp_filename/2,
         update_file/5,
         get_file_size/1,
         get_fd/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {callback}).

-record(file_rev, {last_fetch, % timestamp when the file has last been fetched (by this module)
                   file % undefined or a file descriptor if the file exists
                  }).

-include_lib("records.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Callback) when is_atom(Callback) ->
    GenServerName = Callback:get_storage_gen_server_name(),
    gen_server:start_link({local, GenServerName}, ?MODULE, [Callback], []).

get_file_rev(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    GenServerName = Callback:get_storage_gen_server_name(),
    gen_server:call(GenServerName, {get_file_rev, LocalName}).

is_cached_file_recent(#file_rev{last_fetch=LastFetch}, Callback) ->
    case LastFetch of
        undefined ->
            false;
        _ when is_integer(LastFetch) ->
            FeedRecent = Callback:file_recent(),
            erlang:system_time(seconds) - LastFetch < FeedRecent
    end.

exists_cached_file(#file_rev{file=File}) ->
    case File of
        undefined -> false;
        _ -> true
    end.

%% @doc
%% Creates a filename a worker can open and write to
create_temp_filename(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    Destination = list_to_binary([Callback:get_cached_file_path(LocalName), $.,
                                  integer_to_binary(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(Destination),
    Destination.

update_file(LocalName, NewRevFilePath, OriginalMTime, FetchTime, Callback) when
      is_binary(LocalName), is_integer(OriginalMTime), is_integer(FetchTime), is_atom(Callback) ->
    GenServerName = Callback:get_storage_gen_server_name(),
    gen_server:call(GenServerName, {update_file, LocalName, NewRevFilePath,
                                    OriginalMTime, FetchTime}).

get_file_size(#file_rev{file={FileSize, _}}) when is_integer(FileSize) -> FileSize.
get_fd(#file_rev{file={_, Fd}}) -> Fd.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Callback]) ->
    {ok, #state{callback=Callback}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_file_rev, LocalName}, _From, #state{callback=Callback}=State) when is_binary(LocalName) ->
    Reply = get_file_rev_int(LocalName, Callback),
    {reply, Reply, State};

handle_call({update_file, LocalName, NewRevFilePath, OriginalMTime, FetchTime},
            _From, #state{callback=Callback}=State) when is_binary(LocalName), is_integer(OriginalMTime) ->
    CachedPath = Callback:get_cached_file_path(LocalName),
    ok = podrec_util:move_file(NewRevFilePath, CachedPath),
    ok = podrec_util:set_file_mtime(OriginalMTime, CachedPath),
    ok = update_last_fetch_int(LocalName, FetchTime, Callback),
    {reply, LocalName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({update_last_fetch, LocalName, FetchTime}, #state{callback=Callback}=State) when is_binary(LocalName) ->
    ok = update_last_fetch_int(LocalName, FetchTime, Callback),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_file_rev_int(LocalName, Callback) ->
    case get_file_from_db(LocalName, Callback) of
        {ok, #file{last_fetch=LastFetch}} ->
            FileRev = #file_rev{last_fetch=LastFetch},
            F = FileRev#file_rev{file=get_local_file(LocalName, Callback)},
            F;
        {error, unknown_file} ->
            #file_rev{file=get_local_file(LocalName, Callback)}
    end.

get_file_from_db(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    T = fun() -> mnesia:read(Callback:mnesia_table_name(), LocalName) end,
    case mnesia:transaction(T) of
        {atomic, [File]} ->
            {ok, File};
        {atomic, []} ->
            {error, unknown_file}
    end.

get_local_file(LocalName, Callback) when is_binary(LocalName) ->
    CachedPath = Callback:get_cached_file_path(LocalName),
    case file:open(CachedPath, [read, binary]) of
        {ok, File} ->
            FileSize = filelib:file_size(CachedPath),
            {FileSize, File};
        {error, enoent} ->
            undefined
    end.

update_last_fetch_int(LocalName, FetchTime, Callback) when is_binary(LocalName), is_atom(Callback) ->
    T = fun() -> [File] = mnesia:read(Callback:mnesia_table_name(), LocalName),
                 mnesia:write(Callback:mnesia_table_name(), File#file{last_fetch=FetchTime}, write)
        end,
    {atomic, ok} = mnesia:transaction(T),
    ok.

