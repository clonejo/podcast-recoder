-module(podrec_feeds).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

-behaviour(podrec_files).

%% API
-export([init_file_table/0, start_link/0, start_link_storage/0, get_file/1,
         get_storage_gen_server_name/0, get_feeds/0]).

%% Callbacks
-export([mnesia_table_name/0, try_recode/2, get_cached_file_path/1,
         file_fetch_user_timeout/0, file_recent/0, get_file_preconfigured_url/1,
         get_max_cache_size/0, compressible/0]).

-include_lib("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").

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

get_feeds() ->
    T = fun() -> mnesia:select(mnesia_table_name(), [{#file{local_name='$1', orig_url='$2', _='_'},
                                                       [], [{{'$1', '$2'}}]}]) end,
    {atomic, Feeds} = mnesia:transaction(T),
    Feeds.


%%%===================================================================
%%% Callbacks
%%%===================================================================

mnesia_table_name() -> ?MODULE.

try_recode(OriginalFilePath, RecodedFilePath) ->
    {OriginalXml, _} = xmerl_scan:file(OriginalFilePath),
    RecodedXml = recode_xml(OriginalXml),
    RecodedXmlString = xmerl:export_simple([RecodedXml], xmerl_xml),
    lager:info("writing to ~p", [RecodedFilePath]),
    {ok, File} = file:open(RecodedFilePath, [write, {encoding, utf8}]),
    ok = io:format(File, "~ts~n", [lists:flatten(RecodedXmlString)]),
    ok = file:delete(OriginalFilePath),
    finished.

% rss
recode_xml(#xmlElement{name=rss, parents=[], content=Content}=Elem) ->
    Elem#xmlElement{content=lists:map(fun recode_xml/1, Content)};

% rss/channel
recode_xml(#xmlElement{name=channel, parents=[{rss, _}], content=Content}=Elem) ->
    Elem#xmlElement{content=lists:map(fun recode_xml/1, Content)};

% rss/channel/title
recode_xml(#xmlElement{name=title, parents=[{channel, _}, {rss, _}], content=Content}=Elem) ->
    Elem#xmlElement{content=lists:map(fun recode_xml/1, Content)};

% rss/channel/title/
recode_xml(#xmlText{parents=[{title, _}, {channel, _}, {rss, _}], value=Value}=Text) ->
    Text#xmlText{value=[Value, " [recoded]"]};

% rss/channel/item
recode_xml(#xmlElement{name=item, parents=[{channel, _}, {rss, _}], content=Content}=Elem) ->
    Elem#xmlElement{content=lists:map(fun recode_xml/1, Content)};

% rss/channel/item/enclosure
recode_xml(#xmlElement{name=enclosure, parents=[{item, _}, {channel, _}, {rss, _}],
                       content=Content, attributes=Attributes}=Elem) ->
    {value, #xmlAttribute{value=Url}=UrlAttr} = lists:keysearch(url, 2, Attributes),
    LocalName = convert_to_local_name(Url),
    ok = podrec_enclosures:add_enclosure_to_db(LocalName, list_to_binary(Url)),
    NewUrl = podrec_enclosures:get_file_url(LocalName),
    NewAttributes = lists:keyreplace(url, 2, Attributes, UrlAttr#xmlAttribute{value=NewUrl}),
    NewAttributes2 = lists:keystore(type, 2, NewAttributes,
                                    #xmlAttribute{name=type, value = <<"audio/opus">>}),
    Elem#xmlElement{content=lists:map(fun recode_xml/1, Content), attributes=NewAttributes2};

recode_xml(Unknown) ->
    Unknown.

file_fetch_user_timeout() ->
    podrec_util:get_env(feed_fetch_user_timeout, 60*1000).
file_recent() ->
    podrec_util:get_env(feed_recent, 10*60).

get_cached_file_path(LocalName) when is_binary(LocalName) ->
    filename:join([podrec_util:get_env(cached_feeds_path, <<"feeds_cache">>),
                   list_to_binary([LocalName, <<".xml">>])]).

get_file_preconfigured_url(LocalName) when is_binary(LocalName) ->
            ConfiguredFeeds = podrec_util:get_env(feeds),
            case maps:get(LocalName, ConfiguredFeeds, undefined) of
                undefined -> {error, unknown_file};
                OriginalUrl ->
                    {ok, OriginalUrl}
            end.

convert_to_local_name(Url) ->
    list_to_binary([podrec_util:bin_to_hex(crypto:hash(sha256, Url)), <<".opus">>]).

get_storage_gen_server_name() -> podrec_feeds_storage.

get_max_cache_size() ->
    podrec_util:get_env(max_feeds_cache_size, 1*1024*1024).

compressible() -> true.

