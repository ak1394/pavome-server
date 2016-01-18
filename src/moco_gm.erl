-module(moco_gm).

-include_lib("kernel/include/file.hrl").

-export([resize/6, resize_file/5, montage/5, montage_file/4, rotate/4, rotate_file/3]).

montage(ImageBin, ContentTypeIn, ContentTypeOut, Width, Height) ->
	abintofile(fun montage_file/4, ImageBin, ContentTypeIn, [ContentTypeOut, Width, Height]).
	
montage_file(ImageFile, ContentTypeOut, Width, Height) ->
	FileOut = tmpname(ContentTypeOut),
	verify(FileOut, run_gm(["montage",  "-background", "'#000000FF'", "-geometry", size(Width, Height), ImageFile, FileOut])).

rotate(ImageBin, ContentTypeIn, ContentTypeOut, Rotation) ->
	abintofile(fun rotate_file/3, ImageBin, ContentTypeIn, [ContentTypeOut, Rotation]).

rotate_file(ImageFile, ContentTypeOut, Rotation) ->
	FileOut = tmpname(ContentTypeOut),
	verify(FileOut, run_gm(["convert", ImageFile, "-rotate", integer_to_list(Rotation), FileOut])).

resize(ImageBin, ContentTypeIn, ContentTypeOut, Width, Height, Rotation) ->
	abintofile(fun resize_file/5, ImageBin, ContentTypeIn, [ContentTypeOut, Width, Height, Rotation]).

resize_file(ImageFile, ContentTypeOut, Width, Height, Rotation) ->
	FileOut = tmpname(ContentTypeOut),
	verify(FileOut, run_gm(["convert", ImageFile, "-resize", size(Width, Height), "-rotate", integer_to_list(Rotation), FileOut])).

abintofile(Fun, ImageBin, ContentType, Args) ->
	ImageFile = tmpfile(ContentType, ImageBin),
	ResultFile = apply(Fun, [ImageFile] ++ Args),
	{ok, Result} = file:read_file(ResultFile),
    file:delete(ImageFile),
    file:delete(ResultFile),
	Result.	

tmpname(ContentType) ->
	{ok, TmpName} = moco_util:mktemppath("/tmp", mapext(ContentType)),
	TmpName.

tmpfile(ContentType, ImageBin) ->
	moco_util:writetemp("/tmp", ImageBin, mapext(ContentType)).

size(Width, Height) when is_integer(Width), is_integer(Height) ->
	lists:concat([integer_to_list(Width), "x", integer_to_list(Height)]);

size(Width, undefined) when is_integer(Width) ->
	lists:concat([integer_to_list(Width)]);

size(undefined, Height) when is_integer(Height) ->
	lists:concat(["x", integer_to_list(Height)]).

verify(File, Message) ->
	case file:read_file_info(File) of 
    	{ok, FileInfo} when FileInfo#file_info.size > 0 ->
            File;
        _ ->
            case file:read_file_info(File ++ ".0") of
                {ok, FileInfo} when FileInfo#file_info.size > 0 ->
                    File ++ ".0";
                _ ->
                    io:format("command failed: ~p~n", [[Message]]),
                    undefined
            end
	end.

run_gm(Command) ->
	Cmd = string:join(["/opt/pavo/bin/gm", string:join(Command, " ")], " "),
	os:cmd(Cmd).

mapext("image/bmp") -> ".bmp";
mapext("image/png") -> ".png";
mapext("image/x-png") -> ".png";
mapext("image/gif") -> ".gif";
mapext("image/jpg") -> ".jpg";
mapext("image/jpeg") -> ".jpg";
mapext("image/pjpeg") -> ".jpg".
