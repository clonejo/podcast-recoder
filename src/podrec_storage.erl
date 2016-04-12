-module(podrec_storage).
% Copyright 2015-2016 Feiko Nanninga
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
%    - get_file_rev() returns a file descriptor
%
% The storage servers will clean up the *_cache directories regularly
% (storage_cleanup_interval option). The given max cache sizes are not hard
% limits, but determine how much is deleted during each cleanup.

-behaviour(gen_server).

%% API
-export([start_link/1,
         get_file_rev/2,
         is_cached_file_recent/2,
         exists_cached_file/1,
         create_temp_filename/2,
         update_file/5,
         get_file_size/1,
         get_fd/1,
         get_mtime/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {callback}).

-record(file_rev, {last_fetch,  % timestamp when the file has last been fetched
                   mtime,       % modification time of the original file
                   size,        % undefined or the size of the file if it exists
                   fd           % undefined or a file descriptor if the file exists
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
    lager:debug("get_file_rev(~p,~p)", [LocalName, Callback]),
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

exists_cached_file(#file_rev{fd=File}) ->
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

update_file(LocalName, NewRevFilePath, FetchTime, OriginalMTime, Callback) when
      is_binary(LocalName), is_integer(FetchTime), is_integer(OriginalMTime), is_atom(Callback) ->
    lager:debug("update_file(~p,~p,~p,~p,~p)", [LocalName, NewRevFilePath, FetchTime, OriginalMTime, Callback]),
    GenServerName = Callback:get_storage_gen_server_name(),
    gen_server:call(GenServerName, {update_file, LocalName, NewRevFilePath, FetchTime, OriginalMTime}).

get_file_size(#file_rev{size=FileSize}) when is_integer(FileSize) -> FileSize.
get_fd(#file_rev{fd=Fd}) -> Fd.
get_mtime(#file_rev{mtime=MTime}) -> MTime.


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
    cleanup_int(Callback),
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
    ok = update_last_requested_int(LocalName, Callback),
    {reply, Reply, State};

handle_call({update_file, LocalName, NewRevFilePath, FetchTime, OriginalMTime},
            _From, #state{callback=Callback}=State) when is_binary(LocalName), is_integer(OriginalMTime) ->
    CachedPath = Callback:get_cached_file_path(LocalName),
    lager:info("updating file in cache: ~p, ~p -> ~p", [NewRevFilePath, OriginalMTime, LocalName]),
    ok = podrec_util:move_file(NewRevFilePath, CachedPath),
    ok = podrec_util:set_file_mtime(OriginalMTime, CachedPath),
    T = fun() -> case mnesia:read(Callback:mnesia_table_name(), LocalName) of
                     [File] ->
                         mnesia:write(Callback:mnesia_table_name(), File#file{last_fetch=FetchTime}, write);
                     [] ->
                         ok
                 end
        end,
    {atomic, ok} = mnesia:transaction(T),
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
handle_cast({}, State) ->
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
handle_info(cleanup, #state{callback=Callback}=State) ->
    cleanup_int(Callback),
    {noreply, State};

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
            FileRev = get_local_file(LocalName, Callback),
            FileRev#file_rev{last_fetch=LastFetch};
        {error, unknown_file} ->
            get_local_file(LocalName, Callback)
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
            MTime = podrec_util:get_file_mtime(CachedPath),
            #file_rev{size=FileSize, fd=File, mtime=MTime};
        {error, enoent} ->
            #file_rev{}
    end.

%% @private
%% @doc
%% updates last_fetch entry of a file in the db (if that file exists in db)
update_last_requested_int(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    Time = erlang:monotonic_time(seconds),
    T = fun() -> case mnesia:read(Callback:mnesia_table_name(), LocalName) of
                     [File] ->
                         mnesia:write(Callback:mnesia_table_name(), File#file{last_requested=Time}, write);
                     [] ->
                         ok
                 end
        end,
    {atomic, ok} = mnesia:transaction(T),
    ok.

cleanup_int(Callback) ->
    MaxKBytes = Callback:get_max_cache_size(),
    lager:info("running cleanup (~p), shrinking to ~pMiB", [Callback, MaxKBytes div 1024]),
    T = fun() -> mnesia:select(Callback:mnesia_table_name(), [{#file{local_name='$1', last_requested='$2', _='_'},
                                                               [], [{{'$1', '$2'}}]}]) end,
    {atomic, Files} = mnesia:transaction(T),
    F = fun({LocalName, LastRequested}) ->
                Path = Callback:get_cached_file_path(LocalName),
                case podrec_util:get_file_size(Path) of
                    {error, enoent} ->
                        false;
                    Size ->
                        {true, {LocalName, Path, LastRequested, Size}}
                end
        end,
    FilesWithSizes = lists:filtermap(F, Files),
    SortedFiles = lists:sort(fun({_, _, A, _}, {_, _, B, _}) -> A > B end, FilesWithSizes),
    Now = erlang:monotonic_time(seconds),
    FoldFun = fun({LocalName, Path, LastRequested, Size}, RemainingBytes) ->
                          case RemainingBytes >= Size of
                              true -> % keep file
                                  RemainingBytes - Size;
                              false -> % delete file
                                  lager:info("cleanup (~p): deleting ~p, last requested ~p seconds ago",
                                             [Callback, LocalName, Now - LastRequested]),
                                  ok = file:delete(Path),
                                  RemainingBytes
                          end
                  end,
    R = lists:foldl(FoldFun, MaxKBytes*1024, SortedFiles),
    lager:info("finished cleanup (~p), leftover space: ~pMiB", [Callback, R div 1024 div 1024]),
    erlang:send_after(podrec_util:get_env(storage_cleanup_interval, 60*1000), self(), cleanup).

