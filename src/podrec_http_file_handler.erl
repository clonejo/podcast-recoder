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
    lager:info("1"),
    case podrec_files:get_file(LocalName, Callback) of
        {ok, FileRev} ->
            lager:info("delivering file from cache, FileRev=~p", [FileRev]),
            FileSize = podrec_storage:get_file_size(FileRev),
            {ok, Req4} = cowboy_req:chunked_reply(200, Req2),
            ok = send_file(podrec_storage:get_fd(FileRev), Req4),
            {ok, Req4, State};
        {error, unknown_file} ->
            {ok, Req3} = cowboy_req:reply(404, [], <<"Unknown file\n">>, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

send_file(Fd, Req) ->
    case file:read(Fd, ?CHUNK_SIZE) of
        {ok, Chunk} ->
            ok = cowboy_req:chunk(Chunk, Req),
            send_file(Fd, Req);
        eof ->
            ok
    end.
