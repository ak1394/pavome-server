-module(moco_proto).

-export([encode/2, decode/2, pack_token/3, unpack_token/2, urlbase64/1, random_token/0]).

%%-compile(export_all). 

%% ====================================================================
%% External functions
%% ====================================================================

encode(attached, undefined) -> 0;
encode(attached, image) -> 1;
encode(attached, video) -> 2;
encode(attached, audio) -> 3;
encode(origin, moco) -> 4;
encode(origin, twitter) -> 5;
encode(origin, system) -> 6;
encode(origin, tweetphoto) -> 7.

decode(attached, 0) -> undefined;
decode(attached, 1) -> image;
decode(attached, 2) -> video;
decode(attached, 3) -> audio;
decode(origin, 4) -> moco;
decode(origin, 5) -> twitter;
decode(origin, 6) -> system;
decode(origin, 7) -> tweetphoto.

pack_token(Kind, Id, Secret) ->
    pack_token_v1(encode(attached, Kind), Id, Secret).

pack_token_v1(Kind, Id, Secret) ->
    Mac = crypto:sha_mac_96(Secret, <<Id:40>>),
    base64_to_urlbase64(base64:encode(<<1:4, Kind:4, Id:40, Mac/binary>>)).

unpack_token(Token, Secret) ->
    try unpack_token2(base64:decode(urlbase64_to_base64(Token)), Secret) catch 
        error:_ -> undefined
    end.

unpack_token2(<<1:4, Kind:4, Id:40, Mac/binary>>, Secret) ->
    case crypto:sha_mac_96(Secret, <<Id:40>>) == Mac of
        true -> {decode(attached, Kind), Id};
        false -> undefined
    end;

unpack_token2(_, _Secret) -> undefined.

urlbase64(Data) ->
    base64_to_urlbase64(base64:encode(Data)).

base64_to_urlbase64(Base64) ->
    Result = lists:map(fun(Char) ->
                    case Char of
                        $+ -> $-;
                        $/ -> $_;
                        Else -> Else
                    end
                end, binary_to_list(Base64)),
    list_to_binary(Result).

urlbase64_to_base64(UrlBase64) when is_binary(UrlBase64) ->
    urlbase64_to_base64(binary_to_list(UrlBase64));

urlbase64_to_base64(UrlBase64) when is_list(UrlBase64) ->
    lists:map(fun(Char) ->
                    case Char of
                        $- -> $+;
                        $_ -> $/;
                        Else -> Else
                    end
                end, UrlBase64).

random_token() ->
    moco_proto:urlbase64(<<
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32
    >>).
