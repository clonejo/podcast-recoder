-module(podrec_util).

-export([get_env/1, get_env/2,
         generate_temp_filepath/1,
         move_file/2,
         get_file_mtime/1,
         set_file_mtime/2]).

-include_lib("kernel/include/file.hrl").

get_env(Key) ->
    {ok, Value} = application:get_env(podcast_recoder, Key),
    Value.
get_env(Key, Default) ->
    case application:get_env(podcast_recoder, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

generate_temp_filepath(Extension) ->
    TempDir = podrec_util:get_env(temp_dir, <<"/tmp/podcast_recoder">>),
    filename:join([TempDir, list_to_binary([integer_to_list(erlang:unique_integer([positive])),
                                            $., Extension])]).

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
