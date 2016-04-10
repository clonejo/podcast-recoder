-module(podrec_files).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

-behaviour(gen_server).
%%
%% API
-export([init_file_table/1,
         start_link/1,
         get_file/2,
         exists_cached_file/1,
         update_orig_url/3,
         update_last_requested/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {jobs=#{},
                jobs_running=0,
                queue=queue:new(),
                callback}).

-include_lib("records.hrl").

%%%===================================================================
%%% Behaviour methods
%%%===================================================================

-callback start_link() -> T :: term().

-callback start_link_storage() -> T :: term().

-callback mnesia_table_name() -> TableName :: atom().

-callback get_cached_file_path(LocalName :: binary()) -> Path :: term().

-callback try_recode(OriginalFilePath :: term(), RecodedFilePath :: term()) ->
    finished | {error, Reason :: term()}.

-callback file_fetch_user_timeout() -> Milliseconds :: integer().

-callback file_recent() -> Seconds :: integer().

-callback get_file_preconfigured_url(LocalName :: binary()) ->
    {ok, Path :: term()} | {error, Reason :: term()}.

-callback get_storage_gen_server_name() ->
    GenServerName :: atom().

%%%===================================================================
%%% API
%%%===================================================================

init_file_table(Callback) when is_atom(Callback) ->
    mnesia:create_table(Callback:mnesia_table_name(),
                        [{record_name, file},
                         {attributes, record_info(fields, file)},
                         {disc_copies, [node()]}]),
    ok.

start_link(Callback) when is_atom(Callback) ->
    gen_server:start_link({local, Callback}, ?MODULE, [Callback], []).

get_file(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    FileRev = podrec_storage:get_file_rev(LocalName, Callback),
    R = case podrec_storage:exists_cached_file(FileRev) of
            true ->
                case podrec_storage:is_cached_file_recent(FileRev, Callback) of
                    true ->
                        lager:info("Req:~p cache is recent", [LocalName]),
                        {ok, FileRev};
                    false ->
                        lager:info("Req:~p cache is not recent, trying to fetch ...", [LocalName]),
                        case try_fetch(LocalName, Callback) of
                            {ok, _} ->
                                lager:info("Req:~p fetch successful", [LocalName]),
                                {ok, podrec_storage:get_file_rev(LocalName, Callback)};
                            % FIXME: don't serve older version on 404, return 404 and delete local version
                            {error, Reason} ->
                                lager:info("Req:~p error on fetching, serving older version (error: ~p)",
                                           [LocalName, Reason]),
                                {ok, FileRev} % just serve older version :P
                        end
                end;
            false ->
                lager:info("Req:~p not in cache, trying to fetch ...", [LocalName]),
                case try_fetch(LocalName, Callback) of
                    {ok, NewFileRevPath} ->
                        lager:info("Req:~p fetch successful", [LocalName]),
                        {ok, podrec_storage:get_file_rev(NewFileRevPath, Callback)};
                    {error, Reason} ->
                        lager:info("Req:~p error on fetching, reason: ~p", [LocalName, Reason]),
                        {error, Reason}
                end
        end,
    case R of
        {ok, _} ->
            update_last_requested(LocalName, Callback);
        {error, _Reason} ->
            ok
    end,
    R.

%% @doc Updates a file's orig_url in the database. Creates entry if necessary.
update_orig_url(LocalName, Url, Callback) when is_binary(LocalName), is_binary(Url), is_atom(Callback) ->
    lager:info("adding ~p to database", [Url]),
    T = fun() -> File = case mnesia:read(Callback:mnesia_table_name(), LocalName) of
                            [F] -> F;
                            [] -> #file{local_name=LocalName}
                        end,
                 mnesia:write(Callback:mnesia_table_name(), File#file{orig_url=Url}, write) end,
    {atomic, ok} = mnesia:transaction(T),
    ok.

update_last_requested(LocalName,Callback) when is_binary(LocalName), is_atom(Callback) ->
    Now = erlang:system_time(seconds),
    T = fun() -> [Feed] = mnesia:read(Callback:mnesia_table_name(), LocalName),
                 mnesia:write(Callback:mnesia_table_name(), Feed#file{last_requested=Now}, write)
        end,
    {atomic, ok} = mnesia:transaction(T),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Callback]) when is_atom(Callback) ->
    lager:info("gen_server ~p running", [?MODULE]),
    {ok, #state{callback=Callback}}.

handle_call({try_fetch, LocalName, OriginalUrl}, From, #state{jobs=Jobs, callback=Callback}=State) ->
    NewState = case get_job_entry(LocalName, State#state.jobs) of
                   Job when is_map(Job) ->
                       % we already have a job for this file
                       NewJob = add_notified_client_to_job(From, Job),
                       State#state{jobs=update_job(LocalName, NewJob, Jobs)};
                   undefined ->
                       % no job exists, add it
                       U = case max_workers_running(State#state.jobs_running) of
                               true ->
                                   {queue:in({LocalName, OriginalUrl}, State#state.queue),
                                    State#state.jobs_running};
                               false ->
                                   ok = start_job(LocalName, OriginalUrl, Callback),
                                   {State#state.queue, State#state.jobs_running + 1}
                           end,
                       {NewQueue, NewJobsRunning} = U,
                       lager:info("running: ~p, queue: ~p", [NewJobsRunning,
                                                             queue:len(NewQueue)]),
                       JobEntry = add_notified_client_to_job(From, new_job_entry()),
                       State#state{jobs=add_job_entry(LocalName, JobEntry, Jobs),
                                   jobs_running=NewJobsRunning,
                                   queue=NewQueue}
               end,
    {noreply, NewState}.

handle_cast({worker_terminated, LocalName, Msg}, State) ->
    #{notify_clients := Clients} = get_job_entry(LocalName, State#state.jobs),
    lists:foreach(fun(Client) ->
                          gen_server:reply(Client, Msg) end, Clients),
    NewState = State#state{jobs=remove_job(LocalName, State#state.jobs)},
    {noreply, process_queue(NewState)}.

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


try_fetch(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    case get_file_original_url(LocalName, Callback) of
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

get_file_original_url(LocalName, Callback) when is_binary(LocalName), is_atom(Callback) ->
    T = fun() -> mnesia:read(Callback:mnesia_table_name(), LocalName) end,
    case mnesia:transaction(T) of
        {atomic, [#file{orig_url=DbUrl}]} ->
            case Callback:get_file_preconfigured_url(LocalName) of
                {ok, DbUrl} -> % url in db and config are the same
                    {ok, DbUrl};
                {ok, PreconfiguredUrl} -> % url in db and config differ
                    ok = update_orig_url(LocalName, DbUrl, Callback),
                    {ok, PreconfiguredUrl};
                {error, unknown_file} ->
                    {ok, DbUrl}
            end;
        {atomic, []} ->
            case Callback:get_file_preconfigured_url(LocalName) of
                {ok, PreconfiguredUrl} ->
                    ok = update_orig_url(LocalName, PreconfiguredUrl, Callback),
                    {ok, PreconfiguredUrl};
                {error, unknown_file} ->
                    {error, unknown_file}
            end
    end.

max_workers_running(JobsRunning) ->
    MaxWorkers = podrec_util:get_env(max_workers, 2),
    JobsRunning >= MaxWorkers.

process_queue(#state{jobs_running=JobsRunning, queue=Queue, callback=Callback} = State) ->
    case queue:out(Queue) of
        {empty, _} ->
            lager:info("running: ~p, queue: ~p", [JobsRunning-1, queue:len(Queue)]),
            State#state{jobs_running = JobsRunning - 1};
        {{value, {LocalName, OriginalUrl}}, NewQueue} ->
            lager:info("running: ~p, queue: ~p", [JobsRunning, queue:len(NewQueue)]),
            ok = start_job(LocalName, OriginalUrl, Callback),
            State#state{queue=NewQueue}
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


start_job(LocalName, OriginalUrl, Callback) ->
    podrec_file_worker:start_link(LocalName, OriginalUrl, Callback),
    ok.

