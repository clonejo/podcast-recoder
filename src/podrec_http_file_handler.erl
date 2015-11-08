-module(podrec_http_file_handler).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

% cowboy callbacks
-export([init/3, handle/2, terminate/3]).

-record(state, {callback}).

init(_Type, Req, Opts) ->
    {ok, Req, #state{callback=maps:get(callback, Opts)}}.

handle(Req, #state{callback=Callback}=State) ->
    {LocalName, Req2} = cowboy_req:binding(name, Req),
    case podrec_files:get_file(LocalName, Callback) of
        {ok, Path} ->
            FileSize = filelib:file_size(Path),
            F = fun(Socket, Transport) ->
                        Transport:sendfile(Socket, Path)
                end,
            Req3 = cowboy_req:set_resp_body_fun(FileSize, F, Req2),
            {ok, Req4} = cowboy_req:reply(200, [], Req3),
            {ok, Req4, State};
        {error, unknown_file} ->
            {ok, Req3} = cowboy_req:reply(404, [], <<"Unknown file\n">>, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.


