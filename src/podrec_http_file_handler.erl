-module(podrec_http_file_handler).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

% cowboy callbacks
-export([init/3, handle/2, terminate/3]).

-record(state, {callback}).

-define(CHUNK_SIZE, 1*1024*1024).

init(_Type, Req, Opts) ->
    {ok, Req, #state{callback=maps:get(callback, Opts)}}.

handle(Req, #state{callback=Callback}=State) ->
    {LocalName, Req2} = cowboy_req:binding(name, Req),
    case podrec_files:get_file(LocalName, Callback) of
        {ok, FileRev} ->
            lager:info("delivering file from cache, FileRev=~p", [FileRev]),
            lager:debug("headers:~n~p", [element(1, cowboy_req:headers(Req2))]),
            FileSize = podrec_storage:get_file_size(FileRev),
            MTime = podrec_storage:get_mtime(FileRev),
            Fd = podrec_storage:get_fd(FileRev),
            Compress = Callback:compressible(),
            Req3 = cowboy_req:set_resp_header(<<"last-modified">>, podrec_util:time_format_http(MTime), Req2),
            % so Cowboy only compresses when we give the whole body in cowboy_req:reply
            case Compress of
                false ->
                    Req4 = cowboy_req:set_resp_body_fun(FileSize,
                                                        fun(Socket, Transport) -> send_file(Fd, Socket, Transport) end,
                                                        Req3),
                    {ok, Req5} = cowboy_req:reply(200, [], Req4),
                    {ok, Req5, State};
                true ->
                    Body = podrec_util:read_all(Fd, ?CHUNK_SIZE),
                    {ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
                    {ok, Req4, State}
            end;
        {error, unknown_file} ->
            {ok, Req3} = cowboy_req:reply(404, [], <<"Unknown file\n">>, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

send_file(Fd, Socket, Transport) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Chunk} ->
            ok = Transport:send(Socket, Chunk),
            send_file(Fd, Socket, Transport);
        eof ->
            ok
    end.
