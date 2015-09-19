%%%-------------------------------------------------------------------
%% @doc podcast_recoder public API
%% @end
%%%-------------------------------------------------------------------

-module('podcast_recoder_app').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% shell api
-export([start/0, initdb/0]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    % cowboy init
    Dispatch = cowboy_router:compile([{'_', [{"/", foo, []}
                                             %{'_', cowboy_404_handler, []}]}]),
                                             ]}]),
    {ok, _} = cowboy:start_http(podcast_recoder, podrec_util:get_env(listener_threads, 100),
                                [{port, podrec_util:get_env(port, 8080)}],
                                [{env, [{dispatch, Dispatch}]}]),
    podrec_sup:start_link().

stop(_State) ->
    ok.


%%====================================================================
%% shell API
%%====================================================================

start() ->
    application:ensure_all_started(podcast_recoder).
initdb() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = podrec_feeds:init_feed_table(),
    stopped = mnesia:stop(),
    io:format("Finished setting up db.~n"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
