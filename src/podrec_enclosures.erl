-module(podrec_enclosures).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

-behaviour(podrec_files).

%% API
-export([init_file_table/0, start_link/0, start_link_storage/0, get_file/1, get_file_url/1,
         get_storage_gen_server_name/0, add_enclosure_to_db/2]).

%% Callbacks
-export([mnesia_table_name/0, try_recode/2, get_cached_file_path/1,
         file_fetch_user_timeout/0, file_recent/0, get_file_preconfigured_url/1]).

-include_lib("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init_file_table() ->
    podrec_files:init_file_table(?MODULE).

start_link() ->
    podrec_files:start_link(?MODULE).

start_link_storage() ->
    podrec_storage:start_link(?MODULE).

get_file(LocalName) when is_binary(LocalName) ->
    podrec_files:get_file(LocalName, ?MODULE).

get_file_url(LocalName) when is_binary(LocalName) ->
    BaseUrl = podrec_util:get_env(base_url),
    [BaseUrl, <<"enclosures/">>, LocalName].

add_enclosure_to_db(LocalName, Url) when is_binary(LocalName), is_binary(Url) ->
    ok = podrec_files:update_orig_url(LocalName, Url, ?MODULE).

get_storage_gen_server_name() -> podrec_enclosures_storage.


%%%===================================================================
%%% Callbacks
%%%===================================================================

mnesia_table_name() -> ?MODULE.

% TODO: implement
try_recode(OriginalFilePath, RecodedFilePath) ->
    lager:info("recoding ..."),
    FfmpegPath = podrec_util:get_env(ffmpeg_path, <<"/usr/bin/ffmpeg">>),
    Port = open_port({spawn_executable, FfmpegPath},
                     [{args, [<<"-loglevel">>, <<"error">>,
                              <<"-nostdin">>,
                              <<"-i">>, OriginalFilePath,
                              <<"-f">>, <<"opus">>,
                              <<"-b:a">>, <<"24k">>,
                              RecodedFilePath]}]),
    Timeout = podrec_util:get_env(recoding_timeout, 20*60*1000),
    F = fun F() ->
            receive
                {'EXIT', Port, normal} ->
                    ok = file:delete(OriginalFilePath),
                    finished;
                Msg ->
                    lager:info("ffmpeg: ~p", [Msg]),
                    F()
            after Timeout ->
                ok = podrec_util:kill_port_os_process(Port, 0),
                ok = file:delete(OriginalFilePath),
                ok = file:delete(RecodedFilePath),
                {error, recoding_timeout}
            end
        end,
    F().

get_cached_file_path(LocalName) when is_binary(LocalName) ->
    filename:join([podrec_util:get_env(cached_enclosures_path, <<"enclosures_cache">>),
                   LocalName]).

get_file_preconfigured_url(_LocalName) when is_binary(_LocalName) ->
    {error, unknown_file}.

file_fetch_user_timeout() ->
    podrec_util:get_env(enclosure_fetch_user_timeout, 600000).
file_recent() ->
    podrec_util:get_env(enclosure_recent, 600).
