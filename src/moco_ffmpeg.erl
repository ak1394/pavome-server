-module(moco_ffmpeg).

-include_lib("kernel/include/file.hrl").

-export([convert_3gp_to_flv/1, convert_3gp_to_jpg/2, convert_amr_to_mp4/1,
         convert_3gp_to_3gp_hinted_low/1, convert_amr_to_mp4_hinted_low/1,
         convert_3gp_to_flv_rotated/1, convert_3gp_to_jpg_rotated/2]).

convert_3gp_to_flv(FileIn) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".flv"),
    ffmpeg(["-v 0", "-f 3gp", "-i", FileIn, "-ar 22050", "-f flv"], FileOut).

convert_3gp_to_flv_rotated(FileIn) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".flv"),
    ffmpeg(["-v 0", "-f 3gp", "-i", FileIn, "-ar 22050", "-vfilters \"rotate=90\"", "-f flv"], FileOut).

convert_3gp_to_3gp_hinted_low(FileIn) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".3gp"),
    hint(ffmpeg(["-v 0", "-f 3gp", "-i", FileIn, "-s 128x96", "-r 10", "-b 64k", "-ab 7.4k", "-ar 8000"], FileOut)).

convert_3gp_to_jpg(FileIn, Resolution) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".jpg"),
    ffmpeg(["-v 0", "-f 3gp", "-i", FileIn, "-f mjpeg", "-t 0.001", "-s", Resolution], FileOut).

convert_3gp_to_jpg_rotated(FileIn, Resolution) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".jpg"),
    ffmpeg(["-v 0", "-f 3gp", "-i", FileIn, "-f mjpeg", "-t 0.001", "-s", Resolution, "-vfilters \"rotate=90\""], FileOut).

convert_amr_to_mp4(FileIn) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".mp4"),
    ffmpeg(["-v 0", "-f amr", "-i", FileIn, "-ab 32k"], FileOut).

convert_amr_to_mp4_hinted_low(FileIn) ->
    {ok, FileOut} = moco_util:mktemppath("/tmp", ".mp4"),
    hint_latm(ffmpeg(["-v 0", "-f amr", "-i", FileIn, "-ab 32k"], FileOut)).

ffmpeg(Params, FileOut) ->
    Cmd = string:join(["/opt/pavo/bin/ffmpeg"] ++ Params ++ [FileOut], " "),
    Msg = os:cmd(Cmd),
    io:format("command output: ~p: ~p~n", [Params, Msg]),
    case file:read_file_info(FileOut) of
        {ok, FileInfo} when FileInfo#file_info.size > 0 ->
            FileOut;
        _Else ->
            io:format("command failed: ~p: ~p~n", [Params, Msg]),
            undefined
    end.

hint(Filename) ->
    Cmd = string:join(["/opt/pavo/bin/MP4Box", "-hint", Filename], " "),
    os:cmd(Cmd),
    Filename.

hint_latm(Filename) ->
    Cmd = string:join(["/opt/pavo/bin/MP4Box", "-hint", "-latm", Filename], " "),
    os:cmd(Cmd),
    Filename.
