-module(podrec_feeds).

-behaviour(podrec_files).

%% API
-export([init_feed_table/0, start_link/0, get_feed/1]).

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


%%%===================================================================
%%% Callbacks
%%%===================================================================

mnesia_table_name() -> ?MODULE.

% TODO: implement
try_recode(OriginalFilePath) ->
    {finished, OriginalFilePath}.

file_fetch_user_timeout() ->
    podrec_util:get_env(feed_fetch_user_timeout, 60000).
file_recent() ->
    podrec_util:get_env(feed_recent, 600).

get_cached_file_path(LocalName) when is_binary(LocalName) ->
    filename:join([podrec_util:get_env(cached_feeds_path, <<"feeds_cache">>),
                   list_to_binary([LocalName, <<".xml">>])]).

get_file_preconfigured_url(LocalName) when is_binary(LocalName) ->
            ConfiguredFeeds = podrec_util:get_env(feeds),
            case maps:get(LocalName, ConfiguredFeeds, undefined) of
                undefined -> {error, unknown_feed};
                OriginalUrl ->
                    {ok, OriginalUrl}
            end.
