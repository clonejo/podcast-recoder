-module(podrec_attachments).

-behaviour(podrec_files).

%% API
-export([init_feed_table/0, start_link/0, get_feed/1, get_file_url/1,
         add_attachment_to_db/2]).

%% Callbacks
-export([mnesia_table_name/0, try_recode/1, get_cached_file_path/1,
         file_fetch_user_timeout/0, file_recent/0, get_file_preconfigured_url/1]).

-include_lib("records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init_feed_table() ->
    podrec_files:init_feed_table(?MODULE).

start_link() ->
    podrec_files:start_link(?MODULE).

get_feed(LocalName) when is_binary(LocalName) ->
    podrec_files:get_feed(LocalName, ?MODULE).

get_file_url(LocalName) when is_binary(LocalName) ->
    BaseUrl = podrec_util:get_env(base_url),
    [BaseUrl, <<"attachments/">>, LocalName].

add_attachment_to_db(LocalName, Url) when is_binary(LocalName), is_binary(Url) ->
    ok = podrec_files:add_feed_to_db(#file{local_name=LocalName, orig_url=Url}, ?MODULE).


%%%===================================================================
%%% Callbacks
%%%===================================================================

mnesia_table_name() -> ?MODULE.

% TODO: implement
try_recode(OriginalFilePath) ->
    lager:info("recoding ..."),
    RecodedFilePath = podrec_util:generate_temp_filepath(),
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
                    {finished, RecodedFilePath};
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
    filename:join([podrec_util:get_env(cached_attachments_path, <<"attachments_cache">>),
                   LocalName]).

get_file_preconfigured_url(_LocalName) when is_binary(_LocalName) ->
    {error, unknown_file}.

file_fetch_user_timeout() ->
    podrec_util:get_env(attachment_fetch_user_timeout, 600000).
file_recent() ->
    podrec_util:get_env(attachment_recent, 600).
