% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

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
    % delete leftover files from last run
    TempDir = podrec_util:get_env(temp_dir, <<"/tmp/podcast_recoder">>),
    case ec_file:remove(TempDir, [recursive]) of
        ok -> ok;
        {error, enoent} -> ok
    end,
    % cowboy init
    WebsitePath = podrec_util:get_env(website_path, <<"website.html">>),
    Routes = [{'_', [{"/", cowboy_static, {file, WebsitePath}},
                     {"/feeds/", podrec_http_feedlist, []},
                     {"/feeds/:name", podrec_http_file_handler, #{callback => podrec_feeds}},
                     {"/enclosures/:name", podrec_http_file_handler, #{callback => podrec_enclosures}}
                    ]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http(podcast_recoder, podrec_util:get_env(listener_threads, 100),
                                [{port, podrec_util:get_env(port, 8080)},
                                 {ip, podrec_util:get_env(bind_address, {0,0,0,0,0,0,0,1})}], % listen locally
                                [{env, [{dispatch, Dispatch}]},
                                 {compress, true}]),
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
    ok = podrec_feeds:init_file_table(),
    ok = podrec_enclosures:init_file_table(),
    stopped = mnesia:stop(),
    io:format("Finished setting up db.~n"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
