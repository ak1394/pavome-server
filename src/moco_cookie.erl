-module(moco_cookie).

-export([test/0]).

-compile(export_all).

check(undefined, _Secret) ->
	false;

check(Cookie, Secret) ->
	{Digest, Timestamp, Userid, _Userdata, _Tokens} = parse_cookie(Cookie),	
	NewDigest = list_to_binary(digest({0, 0, 0, 0}, Timestamp, Userid, <<>>, <<>>, Secret)),
	case Digest == NewDigest of
		true ->
			{ok, Userid, Timestamp};
		false ->
			false
	end.

make(Userid, Secret) when is_list(Userid) ->
	make(list_to_binary(Userid),  Secret);

make(Userid, Secret) when is_list(Secret) ->
	make(Userid, list_to_binary(Secret));

make(Userid, Secret) when is_binary(Userid) and is_binary(Secret) ->
	Timestamp = get_unix_timestamp(now()),
	cookie({0, 0, 0, 0}, Timestamp, Userid, <<>>, <<>>, Secret).
	
digest({A, B, C, D} = _Ip, Timestamp, Userid, Tokens, UserData, Secret) ->
    IpBin = <<A:8, B:8, C:8, D:8>>,
    TimestampBin = <<Timestamp:32>>,
    Digest0 = crypto:md5([IpBin, TimestampBin, Secret, Userid, <<0:8>>, Tokens, <<0:8>>, UserData]),
    moco_util:bin2hex(crypto:md5([moco_util:bin2hex(Digest0), Secret])).
	
cookie(Ip, Timestamp, UserId, Tokens, UserData, Secret) ->
    list_to_binary([digest(Ip, Timestamp, UserId, Tokens, UserData, Secret), moco_util:dec2hex(8, Timestamp), UserId, $!, UserData]).
	
get_unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(Now) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).
	
parse_cookie(Cookie) when is_list(Cookie) ->
    parse_cookie(list_to_binary(Cookie));

parse_cookie(Cookie) when is_binary(Cookie) ->
    <<Digest:32/binary, HexTimestamp:8/binary, Rest/binary>> = Cookie,
    S = size(Rest) - 1 ,
    case Rest of
        <<Userid:S/binary, $!>> ->
            {Digest, moco_util:hex2dec(HexTimestamp), Userid, <<>>, <<>>};
        _ ->
            invalid %% won't try to parse userdata for now
    end.

test() ->
	Cookie = <<"49492f861bae2196f47a282a0ef4655849d8c239anton@no-mad.net!">>,
	{_Digest, Timestamp, _Userid, _Userdata, _Tokens} = parse_cookie(Cookie),
	NewCookie = cookie({0, 0, 0, 0}, Timestamp, <<"anton@no-mad.net">>, <<>>, <<>>, <<"djlke4833xx5">>),
	Cookie == NewCookie.
