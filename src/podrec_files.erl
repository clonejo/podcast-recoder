-module(podrec_files).

-behaviour(gen_server).
%%
%% API
-export([init_feed_table/1,
         start_link/1,
         get_file/2,
         exists_cached_file/1,
         add_feed_to_db/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {jobs=#{}, callback}).

-include_lib("records.hrl").

%%%===================================================================
%%% Behaviour methods
%%%===================================================================

-callback mnesia_table_name() -> TableName :: atom().

-callback get_cached_file_path(LocalName :: binary()) -> Path :: term().

-callback try_recode(OriginalFilePath :: term()) ->
    {finished, RecodedFilePath :: term()} | {error, Reason :: term()}.

-callback file_fetch_user_timeout() -> Milliseconds :: integer().

-callback file_recent() -> Seconds :: integer().

-callback get_file_preconfigured_url(LocalName :: binary()) ->
    {ok, Path :: term()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

init_feed_table(Callback) when is_atom(Callback) ->
    mnesia:create_table(Callback:mnesia_table_name(),
                        [{record_name, file},
                         {attributes, record_info(fields, file)},
                         {disc_copies, [node()]}]),
    ok.

start_link(Callback) when is_atom(Callback) ->
    gen_server:start_link({local, Callback}, ?MODULE, [Callback], []).

get_file(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    CachedPath = Callback:get_cached_file_path(LocalName),
    case exists_cached_file(CachedPath) of
        true ->
            case is_cached_file_recent(LocalName, Callback) of
                true ->
                    lager:info("Req:~p cache is recent", [LocalName]),
                    {ok, CachedPath};
                false ->
                    lager:info("Req:~p cache is not recent, trying to fetch ...", [LocalName]),
                    case try_fetch(LocalName, Callback) of
                        {ok, CachedPath} ->
                            lager:info("Req:~p fetch successful", [LocalName]),
                            {ok, CachedPath};
                        {error, Reason} ->
                            lager:info("Req:~p error on fetching, serving older version (error: ~p)", [LocalName, Reason]),
                            {ok, CachedPath} % just serve older version :P
                    end
            end;
        false ->
            lager:info("Req:~p not in cache, trying to fetch ...", [LocalName]),
            case try_fetch(LocalName, Callback) of
                {ok, CachedPath} ->
                    lager:info("Req:~p fetch successful", [LocalName]),
                    {ok, CachedPath};
                {error, Reason} ->
                    lager:info("Req:~p error on fetching, reason: ~p", [LocalName, Reason]),
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Callback]) when is_atom(Callback) ->
    lager:info("gen_server ~p running", [?MODULE]),
    {ok, #state{callback=Callback}}.

handle_call({try_fetch, LocalName, OriginalUrl}, From, #state{jobs=Jobs, callback=Callback}=State) ->
    NewState = case get_job_entry(LocalName, State#state.jobs) of
                   Job when is_map(Job) ->
                       NewJob = add_notified_client_to_job(From, Job),
                       State#state{jobs=update_job(LocalName, NewJob, Jobs)};
                   undefined ->
                       ok = start_job(LocalName, OriginalUrl, Callback),
                       JobEntry = add_notified_client_to_job(From, new_job_entry()),
                       State#state{jobs=add_job_entry(LocalName, JobEntry, Jobs)}
               end,
    {noreply, NewState}.

handle_cast({worker_terminated, LocalName, Msg}, #state{callback=Callback}=State) ->
    case Msg of
        {ok, _} ->
            ok = update_feed_recency(LocalName, Callback);
        {error, _Reason} ->
            ok
    end,
    #{notify_clients := Clients} = get_job_entry(LocalName, State#state.jobs),
    lists:foreach(fun(Client) ->
                          gen_server:reply(Client, Msg) end, Clients),
    {noreply, State#state{jobs=remove_job(LocalName, State#state.jobs)}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

exists_cached_file(Path) ->
    filelib:is_regular(Path).

is_cached_file_recent(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    T = fun() -> mnesia:read(Callback:mnesia_table_name(), LocalName) end,
    case mnesia:transaction(T) of
        {atomic, [#file{last_fetch=LastFetch}]} ->
            % TODO: LastFetch might be undefined
            case LastFetch of
                undefined ->
                    false;
                _ when is_integer(LastFetch) ->
                    FeedRecent = Callback:file_recent(),
                    erlang:system_time(seconds) - LastFetch < FeedRecent
            end;
        {atomic, []} ->
            false
    end.

update_feed_recency(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    Now = erlang:system_time(seconds),
    T = fun() -> [Feed] = mnesia:read(Callback:mnesia_table_name(), LocalName),
                 mnesia:write(Callback:mnesia_table_name(), Feed#file{last_fetch=Now}, write)
        end,
    {atomic, ok} = mnesia:transaction(T),
    ok.

try_fetch(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    case get_feed_original_url(LocalName, Callback) of
        {ok, OriginalUrl} ->
            Timeout = Callback:file_fetch_user_timeout(),
            % though we time out after a while, the job might still continue,
            % updating the cached version
            try gen_server:call(Callback, {try_fetch, LocalName, OriginalUrl}, Timeout)
            catch
                exception: Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_feed_original_url(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    T = fun() -> mnesia:read(Callback:mnesia_table_name(), LocalName) end,
    case mnesia:transaction(T) of
        {atomic, [#file{orig_url=DbUrl}]} ->
            case Callback:get_file_preconfigured_url(LocalName) of
                {ok, DbUrl} -> % url in db and config are the same
                    {ok, DbUrl};
                {ok, PreconfiguredUrl} -> % url in db and config differ
                    ok = add_feed_to_db(#file{local_name=LocalName, orig_url=DbUrl}, Callback),
                    {ok, PreconfiguredUrl};
                {error, unknown_feed} ->
                    {ok, DbUrl}
            end;
        {atomic, []} ->
            case Callback:get_file_preconfigured_url(LocalName) of
                {ok, PreconfiguredUrl} ->
                    ok = podrec_files:add_feed_to_db(#file{local_name=LocalName, orig_url=PreconfiguredUrl},
                                                     Callback),
                    {ok, PreconfiguredUrl};
                {error, unknown_feed} ->
                    {error, unknown_feed}
            end
    end.

add_feed_to_db(Feed, Callback) when is_record(Feed, file), is_atom(Callback) ->
    lager:info("adding ~p to database", [Feed#file.local_name]),
    T = fun() -> mnesia:write(Callback:mnesia_table_name(), Feed, write) end,
    {atomic, ok} = mnesia:transaction(T),
    ok.


% job entry functions

new_job_entry() ->
    #{notify_clients => []}.

add_notified_client_to_job(Client, #{notify_clients := Clients}=Job) ->
    Job#{notify_clients := [Client|Clients]}.


% job map functions

add_job_entry(LocalName, JobEntry, Jobs) when is_map(JobEntry) ->
    maps:put(LocalName, JobEntry, Jobs).

get_job_entry(LocalName, Jobs) ->
    maps:get(LocalName, Jobs, undefined).

update_job(LocalName, NewJob, Jobs) ->
    maps:update(LocalName, NewJob, Jobs).

remove_job(LocalName, Jobs) ->
    maps:remove(LocalName, Jobs).


start_job(LocalName, OriginalUrl, Callback) ->
    podrec_file_worker:start_link(LocalName, OriginalUrl, Callback),
    ok.

