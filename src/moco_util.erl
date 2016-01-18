%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Jun 2008
%%% -------------------------------------------------------------------
-module(moco_util).

-include("moco.hrl").

-export([unix_timestamp/1, mktemppath/1, mktemppath/2, writetemp/2, writetemp/3,
         genname/1, hex2dec/1, bin2hex/1, dec2hex/2, join/2, message_to_proplist/1,
         format_unix_timestamp/1, to_list/1, to_atom/1, to_existing_atom/1, to_binary/1, to_integer/1]).

%% ====================================================================
%% External functions
%% ====================================================================

message_to_proplist(Message) ->
    [{id, Message#moco_message.id},
     {topic_id, Message#moco_message.topic_id},
     {origin, Message#moco_message.origin},
     {profile, Message#moco_message.profile},
     {reference, Message#moco_message.reference},
     {author, Message#moco_message.author},
     {forwarded_by, Message#moco_message.forwarded_by},
     {author_id, Message#moco_message.author_id},
     {irt_reference, Message#moco_message.irt_reference},
     {irt_user, Message#moco_message.irt_user},
     {irt_user_id, Message#moco_message.irt_user_id},
     {posted, Message#moco_message.posted},
     {body, Message#moco_message.body},
     {attachment, Message#moco_message.attachment},
     {attached, Message#moco_message.attached},
     {token, binary_to_list(moco_proto:pack_token(Message#moco_message.attached, Message#moco_message.id, moco:property(web, secret)))},
     {posted_rel, format_unix_timestamp(Message#moco_message.posted)}].

unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(Now) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).       

datetime_from_timestamp(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) + Timestamp).

format_unix_timestamp(Timestamp) ->
    SecondsDiff = unix_timestamp(now()) - Timestamp,
    {Days, Time} = calendar:seconds_to_daystime(SecondsDiff),
    case Days > 28 of
        true -> english_date(Timestamp);
        false -> relative_english_date(Days, Time) ++ " ago"
    end.

plural(Unit, 0) -> "0 " ++ Unit ++ "s";
plural(Unit, 1) -> "1 " ++ Unit;
plural(Unit, Number) -> integer_to_list(Number) ++ " " ++ Unit ++ "s".

english_date(Timestamp) ->
    {{Year, Month, Day} , _Time} = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) + Timestamp),
    integer_to_list(Day) ++ " " ++ httpd_util:month(Month) ++ " " ++ integer_to_list(Year).

relative_english_date(Days, _Time) when Days > 7 ->
    Weeks = Days div 7,
    case Days rem 7 of
        0 ->
            plural("week", Weeks);
        RemDays ->
            plural("week", Weeks) ++ ", " ++ plural("day", RemDays)
    end;

relative_english_date(Days, {0, _Minutes, _Seconds}) when Days > 0 ->
    erlang:integer_to_list(Days) ++ " days";

relative_english_date(Days, {Hours, _Minutes, _Seconds}) when Days > 0 ->
    plural("day", Days) ++ ", " ++ plural("hour", Hours);

relative_english_date(0, {Hours, 0, _Seconds}) when Hours > 0 ->
    plural("hour", Hours);

relative_english_date(0, {Hours, Minutes, _Seconds}) when Hours > 0 ->
    plural("hour", Hours) ++ ", " ++ plural("min", Minutes);

relative_english_date(0, {_Hours, Minutes, _Seconds}) when Minutes > 0 ->
    plural("min", Minutes);

relative_english_date(0, {0, 0, _Seconds}) ->
    "few seconds".

isalphanum(C) when C > 47, C < 58; C > 64, C < 91; C > 96, C < 123 -> true;
isalphanum(_) -> false.

%% Generate a temporary file name of length N
genname(N) -> genname(N, []).
genname(0, L) -> L;
genname(N, L) ->
    R = random:uniform(123),
    case isalphanum(R) of
        true -> genname(N-1, [R|L]);
        false -> genname(N, L)
    end.

%% Returns a randomly generated temporary file path where the basename is
%% of length N
blindmktemppath(Prefix, N) -> Prefix ++ "/" ++ genname(N, []) ++ ".tmp".

mktemppath(Prefix) -> mktemppath2(Prefix, 3, []).
mktemppath(Prefix, Extension) -> mktemppath2(Prefix, 3, Extension).

mktemppath2(_Prefix, 0, _Extension) ->
  {error, to_many_attempts};

mktemppath2(Prefix, Tries, Extension) ->
    Tmp = blindmktemppath(Prefix, 16),
    case file:read_file_info(Tmp ++ Extension) of
        {error, enoent} ->
            {ok, Tmp ++ Extension};
        _ ->
            mktemppath2(Prefix, Tries-1, Extension)
    end.

writetemp(Prefix, Binary) ->
    writetemp(Prefix, Binary, []).

writetemp(Prefix, Binary, Extension) ->
    {ok, Name} =  mktemppath(Prefix, Extension),
    ok = file:write_file(Name, Binary),
    Name.

join([H], _Sep) -> [H];
join([H | T], Sep) -> [H | [[Sep, S] || S <- T]].

%% ------------------------------------------------------------
%% Hexadecimal to Decimal converter
%%

hex2dec(Hex) when is_binary(Hex) -> hex2dec(binary_to_list(Hex));
hex2dec(Hex) when is_list(Hex) -> hex2dec(Hex,0).

hex2dec([H|T],N) when H>=$0,H=<$9 ->
    hex2dec(T,(N bsl 4) bor (H-$0));
hex2dec([H|T],N) when H>=$a,H=<$f ->
    hex2dec(T,(N bsl 4) bor (H-$a+10));
hex2dec([H|T],N) when H>=$A,H=<$F ->
    hex2dec(T,(N bsl 4) bor (H-$A+10));
hex2dec([],N) -> N.


%% ------------------------------------------------------------
%% Decimal to Hex converter
%% M is number of digits we want
%% N is the decimal to be converted

bin2hex(Binary) when is_binary(Binary) -> [dec2hex(2, Byte) || Byte <- binary_to_list(Binary)].

dec2hex(M,N) -> dec2hex(M,N,[]).

dec2hex(0,_N,Ack) -> Ack;
dec2hex(M,N,Ack) -> dec2hex(M-1,N bsr 4,[d2h(N band 15)|Ack]).

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.


to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V).

to_atom(V) when is_list(V) ->
    list_to_atom(V);
to_atom(V) when is_binary(V) ->
    binary_to_atom(V, latin1);
to_atom(V) when is_atom(V) ->
    V.

to_existing_atom(V) when is_list(V) ->
    list_to_existing_atom(V);
to_existing_atom(V) when is_binary(V) ->
    binary_to_existing_atom(V, latin1);
to_existing_atom(V) when is_atom(V) ->
    V.

to_binary(V) when is_list(V) ->
    unicode:characters_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
    V.

to_integer(V) when is_list(V) ->
    list_to_integer(V);
to_integer(V) when is_binary(V) ->
    list_to_integer(binary_to_list(V));
to_integer(V) when is_integer(V) ->
    V.
