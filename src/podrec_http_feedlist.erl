-module(podrec_http_feedlist).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

% cowboy callbacks
-export([init/3, handle/2, terminate/3]).

-record(state, {}).

init(_Type, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    RawFeeds = podrec_feeds:get_feeds(),
    BaseUrl = [podrec_util:get_env(base_url, <<"http://localhost:8080/">>), <<"feeds/">>],
    Feeds = lists:map(fun({F, OriginalUrl}) ->
                              Url = list_to_binary([BaseUrl, F]),
                              #{"name" => F, "url" => Url, "orig_url" => OriginalUrl}
                      end,
                      RawFeeds),
    SortedFeeds = lists:sort(fun(#{"name" := A}, #{"name" := B}) -> A =< B end, Feeds),
    TemplatePath = list_to_binary([code:priv_dir(podcast_recoder), "/templates/feeds.html"]),
    Template = bbmustache:parse_file(TemplatePath),
    Out = bbmustache:compile(Template, #{"feeds" => SortedFeeds}),

    {ok, Req2} = cowboy_req:reply(200, [], Out, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

