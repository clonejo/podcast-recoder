-module(podrec_feed_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {job_manager_pid, local_name}).

start_link(LocalName, OriginalUrl) ->
    gen_server:start_link(?MODULE, [self(), LocalName, OriginalUrl], []).


init([JobManagerPid, LocalName, OriginalUrl]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), {try_fetch, OriginalUrl}),
    {ok, #state{job_manager_pid=JobManagerPid, local_name=LocalName}}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast({try_fetch, OriginalUrl}, #state{local_name=LocalName}=State) ->
    CachedPath = podrec_feeds:get_cached_file_path(LocalName),
    RequestResult = case podrec_feeds:exists_cached_file(CachedPath) of
                        true ->
                            MTime = podrec_util:get_file_mtime(CachedPath),
                            request_original_file(OriginalUrl, MTime);
                        false ->
                            request_original_file(OriginalUrl, undefined)
                    end,
    ReturnMsg = case RequestResult of
                    not_modified ->
                        lager:info("Req:~p request successful, not modified", [LocalName]),
                        {ok, CachedPath};
                    {finished, OriginalFilePath, OriginalMTime} ->
                        case try_recode(OriginalFilePath) of
                            {finished, RecodedFilePath} ->
                                ok = podrec_util:move_file(RecodedFilePath, CachedPath),
                                ok = podrec_util:set_file_mtime(OriginalMTime, CachedPath),
                                {ok, CachedPath};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end,
    gen_server:cast(State#state.job_manager_pid, {worker_terminated, LocalName, ReturnMsg}),
    {stop, normal, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        normal -> ok;
        _ ->
            gen_server:cast(State#state.job_manager_pid,
                            {worker_terminated, State#state.local_name, {error, Reason}})
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request_original_file(OriginalUrl, CachedMTime) when is_binary(OriginalUrl) ->
    % we cannot use httpc's stream to file feature here, as httpc then won't
    % tell us the reply headers
    ReqHeaders = case CachedMTime of
                     undefined -> [];
                     CachedMTime when is_integer(CachedMTime) ->
                         [{"if-modified-since", time_format_http(CachedMTime)}]
                 end,
    {ok, RequestId} = httpc:request(get, {binary_to_list(OriginalUrl), ReqHeaders}, [],
                             [{sync, false}, {stream, self}]),
    OriginalFilePath = podrec_util:generate_temp_filepath(<<"xml">>),
    case write_streamed_data_to_file(RequestId, OriginalFilePath) of
        {finished, Headers} ->
            OriginalMTime = case proplists:get_value("last-modified", Headers) of
                                undefined ->
                                    erlang:system_time(seconds);
                                Str when is_list(Str) ->
                                    qdate:to_unixtime(Str)
                            end,
            {finished, OriginalFilePath, OriginalMTime};
        not_modified ->
            not_modified
    end.


time_format_http(Time) ->
    qdate:to_string("D, d M Y H:i:s", Time) ++ " GMT".

write_streamed_data_to_file(RequestId, FilePath) ->
    ok = filelib:ensure_dir(FilePath),
    {ok, File} = file:open(FilePath, [write]),
    (fun F() ->
         receive
             {http, {RequestId, stream_start, _Headers}} ->
                 F();
             {http, {RequestId, stream, BodyPart}} ->
                 file:write(File, BodyPart),
                 F();
             {http, {RequestId, stream_end, Headers}} ->
                 ok = file:close(File),
                 {finished, Headers};
             {http, {RequestId, {{_, 304, _}, _Headers, <<>>}}} -> % "Not Modified"
                 not_modified;
             Msg ->
                 lager:error("msg: ~p", [Msg]),
                 F()
         end
     end)().

% TODO: implement
try_recode(OriginalFilePath) ->
    {finished, OriginalFilePath}.
