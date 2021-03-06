-module(podrec_util).
% Copyright 2015 Feiko Nanninga
% This file is part of podcast_recoder, a project licensed under the terms of
% the GNU Affero General Public License Version 3 (see LICENSE).

-export([get_env/1, get_env/2,
         set_env/2,
         generate_temp_filepath/0,
         move_file/2,
         get_file_mtime/1,
         set_file_mtime/2,
         get_file_size/1,
         kill_port_os_process/2,
         bin_to_hex/1,
         time_format_http/1,
         debug_format_time/1,
         read_all/2]).

-include_lib("kernel/include/file.hrl").

get_env(Key) ->
    {ok, Value} = application:get_env(podcast_recoder, Key),
    Value.
get_env(Key, Default) ->
    case application:get_env(podcast_recoder, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

set_env(Key, Value) when is_atom(Key) ->
    ok = application:set_env(podcast_recoder, Key, Value).

generate_temp_filepath() ->
    TempDir = podrec_util:get_env(temp_dir, <<"/tmp/podcast_recoder">>),
    filename:join([TempDir, integer_to_binary(erlang:unique_integer([positive]))]).

move_file(Source, Destination) ->
    ok = filelib:ensure_dir(Destination),
    case file:rename(Source, Destination) of
        ok -> ok;
        {error, exdev} ->
            {ok, _} = file:copy(Source, Destination),
            ok = file:delete(Source)
    end.

get_file_mtime(Path) ->
    {ok, FileInfo} = file:read_file_info(Path, [{time, posix}]),
    FileInfo#file_info.mtime.

set_file_mtime(MTime, Path) ->
    {ok, FileInfo} = file:read_file_info(Path, [{time, posix}]),
    NewFileInfo = FileInfo#file_info{mtime=MTime},
    ok = file:write_file_info(Path, NewFileInfo, [{time, posix}]).

get_file_size(Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo} ->
            FileInfo#file_info.size
    end.

kill_port_os_process(Port, KillTimeout) when is_port(Port) ->
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    case os:cmd(io_lib:format("kill -15 ~B", [OsPid])) of
        "" ->
            receive
                {'EXIT', Port, normal} ->
                    ok;
                {'EXIT', Port, _} = Msg ->
                    lager:info("unknown message after kill -15: ~p", [Msg]),
                    exit(kill)
            after KillTimeout ->
                      "" = os:cmd(io_lib:format("kill -9 ~B", [OsPid])),
                      receive
                          {'EXIT', Port, normal} ->
                              ok;
                          {'EXIT', Port, _} = Msg ->
                              lager:info("unknown message after kill -9: ~p", [Msg]),
                              exit(kill)
                      end
            end;
        Msg ->
            lager:error("something went wrong when killing (~B): ~p", [OsPid, Msg]),
            {error, killing}
    end.

bin_to_hex(Bin) when is_binary(Bin) ->
    list_to_binary([io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Bin]).

debug_format_time(Timestamp) ->
    Seconds = Timestamp rem 60,
    R = case Timestamp div 60 of
            0 -> [integer_to_list(Seconds), "s"];
            Minutes ->
                case Minutes rem 60 of
                    0 ->
                        [integer_to_list(Minutes), "min", integer_to_list(Seconds), "s"];
                    RMinutes ->
                        Hours = Minutes div 60,
                        [integer_to_list(Hours), "h", integer_to_list(RMinutes), "min", integer_to_list(Seconds), "s"]
                end
        end,
    list_to_binary(R).


time_format_http(Time) ->
    qdate:to_string("D, d M Y H:i:s", Time) ++ " GMT".

read_all(Fd, ChunkSize) ->
    read_all(Fd, ChunkSize, []).
read_all(Fd, ChunkSize, SoFar) ->
    case file:read(Fd, ChunkSize) of
        {ok, Chunk} ->
            read_all(Fd, ChunkSize, [Chunk|SoFar]);
        eof ->
            lists:reverse(SoFar)
    end.
