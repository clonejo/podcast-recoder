-module(podrec_attachments).

-behaviour(podrec_file_type).

%% API
-export([init_feed_table/0, start_link/0, get_feed/1]).

%% Callbacks
-export([mnesia_table_name/0, try_recode/1, get_cached_file_path/1,
         file_fetch_user_timeout/0, file_recent/0, get_file_preconfigured_url/1]).

%%%===================================================================
%%% API
%%%===================================================================

init_feed_table() ->
    podrec_files:init_feed_table(?MODULE).

start_link() ->
    podrec_files:start_link(?MODULE).

get_feed(LocalName) when is_binary(LocalName) ->
    podrec_files:get_feed(LocalName, ?MODULE).

file_fetch_user_timeout() ->
    podrec_util:get_env(attachment_fetch_user_timeout, 60000).
file_recent() ->
    podrec_util:get_env(attachment_recent, 600).


%%%===================================================================
%%% Callbacks
%%%===================================================================

mnesia_table_name() -> ?MODULE.

% TODO: implement
try_recode(OriginalFilePath) ->
    {finished, OriginalFilePath}.

get_cached_file_path(LocalName) when is_binary(LocalName) ->
    filename:join([podrec_util:get_env(cached_attachments_path, <<"attachments_cache">>),
                   list_to_binary([LocalName, <<".xml">>])]).

get_file_preconfigured_url(_LocalName) when is_binary(_LocalName) ->
    {error, unknown_feed}.
