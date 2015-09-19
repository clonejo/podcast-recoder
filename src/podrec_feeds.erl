-module(podrec_feeds).

-behaviour(gen_server).

%% API
-export([init_feed_table/0, start_link/0, get_feed/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {jobs=#{}}).

-record(podrec_feed, {local, orig_url}).

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

init_feed_table() ->
    mnesia:create_table(podrec_feed, [{attributes, record_info(fields, podrec_feed)},
                                      {disc_copies, [node()]}]),
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_feed(LocalName) when is_binary(LocalName) ->
    CachedPath = get_cached_file_path(LocalName),
    case exists_cached_file(CachedPath) of
        true ->
            case is_cached_file_recent(CachedPath) of
                true ->
                    lager:info("Req:~p cache is recent", [LocalName]),
                    {ok, CachedPath};
                false ->
                    lager:info("Req:~p cache is not recent, trying to fetch ...", [LocalName]),
                    case try_fetch(LocalName) of
                        {ok, CachedPath} ->
                            lager:info("Req:~p fetch successful", [LocalName]),
                            {ok, CachedPath};
                        {error, _Reason} ->
                            lager:info("Req:~p error on fetching, serving older version", [LocalName]),
                            {ok, CachedPath} % just serve older version :P
                    end
            end;
        false ->
            lager:info("Req:~p not in cache, trying to fetch ...", [LocalName]),
            case try_fetch(LocalName) of
                {ok, CachedPath} ->
                    lager:info("Req:~p fetch successful", [LocalName]),
                    {ok, CachedPath};
                {error, Reason} ->
                    lager:info("Req:~p error on fetching", [LocalName]),
                    {error, Reason}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    lager:info("gen_server ~p running", [?MODULE]),
    {ok, #state{}}.

handle_call({try_fetch, LocalName, OriginalUrl}, From, #state{jobs=Jobs}=State) ->
    NewState = case get_job_entry(LocalName, State#state.jobs) of
                   Job when is_map(Job) ->
                       NewJob = add_notified_client_to_job(From, Job),
                       State#state{jobs=update_job(LocalName, NewJob, Jobs)};
                   undefined ->
                       ok = start_job(LocalName, OriginalUrl),
                       JobEntry = add_notified_client_to_job(From, new_job_entry()),
                       State#state{jobs=add_job_entry(LocalName, JobEntry, Jobs)}
               end,
    {noreply, NewState}.

handle_cast({worker_terminated, LocalName, Msg}, State) ->
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

get_cached_file_path(LocalName) when is_bitstring(LocalName) ->
    filename:join([podrec_util:get_env(cached_feeds_path, <<"feeds_cache">>),
                   list_to_binary([LocalName, <<".xml">>])]).

exists_cached_file(Path) ->
    filelib:is_regular(Path).

is_cached_file_recent(Path) ->
    % TODO: do not use mtime here, bc that's not updated on each successful fetch!
    % instead, store erlang system time with feed in db and check against that
    FeedRecent = podrec_util:get_env(feed_recent, 600),
    MTime = podrec_util:get_file_mtime(Path),
    os:system_time(seconds) - MTime < FeedRecent.

try_fetch(LocalName) ->
    case get_feed_original_url(LocalName) of
        {ok, OriginalUrl} ->
            Timeout = podrec_util:get_env(feed_fetch_user_timeout, 60000),
            % though we time out after a while, the job might still continue,
            % updating the cached version
            gen_server:call(?MODULE, {try_fetch, LocalName, OriginalUrl}, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

get_feed_original_url(LocalName) ->
    % this will need to change for implementing paged feeds
    Feeds = podrec_util:get_env(feeds),
    case maps:get(LocalName, Feeds, undefined) of
        undefined -> {error, unknown_feed};
        OriginalUrl -> {ok, OriginalUrl}
    end.


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


start_job(LocalName, OriginalUrl) ->
    podrec_feed_worker:start_link(LocalName, OriginalUrl),
    ok.


