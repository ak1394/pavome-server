-module(moco_mobile_packet).

-include("moco.hrl").
-include("moco_mobile_packet.hrl").

-export([unpack_params/1,
		 new_message/3,
		 topic_page/2,
		 ok/1,
		 error/1,
		 ping/1,
         pong/1,
		 error_message/2,
		 params/2,
		 settings/1,
		 check_request/1]).

unpack_params(<<_Size:32, Rest/binary>>) ->
    unpack_params(Rest, []).

unpack_params(<<>>, Acc) ->
    Acc;

unpack_params(Binary, Acc) ->
    {Key, Rest1} = unpack_key(Binary),
    {Value, Rest2} = unpack_value(Rest1),
    unpack_params(Rest2, [{Key, Value} | Acc]).

unpack_key(<<KeySize:16, KeyBinary:KeySize/binary, Rest/binary>>) ->
    Key = list_to_atom(binary_to_list(KeyBinary)),
    {Key, Rest}.

unpack_value(<<?PARAM_BIN:8, ValueSize:32, Value:ValueSize/binary, Rest/binary>>) ->
    {Value, Rest};

unpack_value(<<?PARAM_STR:8, ValueSize:16, Value:ValueSize/binary, Rest/binary>>) ->
    {Value, Rest};
    
unpack_value(<<?PARAM_INT:8, Value:32, Rest/binary>>) ->
    {Value, Rest};

unpack_value(<<?PARAM_LONG:8, Value:64, Rest/binary>>) ->
    {Value, Rest};

unpack_value(<<?PARAM_BOOL:8, 1:8, Rest/binary>>) ->
    {true, Rest};

unpack_value(<<?PARAM_BOOL:8, 0:8, Rest/binary>>) ->
    {false, Rest};

unpack_value(<<?PARAM_STR_LIST:8, Size:32, Rest/binary>>) ->
    unpack_str_list(Size, Rest);

unpack_value(<<?PARAM_INT_LIST:8, Size:32, Rest/binary>>) ->
    unpack_int_list(Size, Rest).

unpack_str_list(Size, Binary) ->
    unpack_str_list(Size, Binary, []).

unpack_str_list(0, Rest, Acc) ->
    {Acc, Rest};

unpack_str_list(Size, <<ValueSize:16, Value:ValueSize/binary, Rest/binary>>, Acc) ->
    unpack_str_list(Size - 1, Rest, [Value | Acc]).

unpack_int_list(Size, Binary) ->
    unpack_int_list(Size, Binary, []).

unpack_int_list(0, Rest, Acc) ->
    {Acc, Rest};

unpack_int_list(Size, <<Value:32, Rest/binary>>, Acc) ->
    unpack_int_list(Size - 1, Rest, [Value | Acc]).

new_message({Kind, Name}, Count, Hush) -> 
    ParamsBin = params([{str, name, Name}, {str, kind, Kind}, {int, count, Count}, {bool, hush, Hush}]),
    <<?PACKET_TOPIC_MESSAGE:8, 0:32, ParamsBin/binary>>.

topic_page(Id, []) ->
    <<?PACKET_TOPIC_PAGE:8, Id:32, 0:8>>;

topic_page(Id, Messages) ->
	IdList  = lists:map(fun(Message) -> Message#moco_message.id end, Messages),
	ReferenceList  = lists:map(fun(Message) -> Message#moco_message.reference end, Messages),
	BodyList = lists:map(fun(Message) -> Message#moco_message.body end, Messages),
	AuthorList = lists:map(fun(Message) -> Message#moco_message.author end, Messages),
	AuthorIdList = lists:map(fun(Message) -> Message#moco_message.author_id end, Messages),
	ForwardedByList = lists:map(fun(Message) -> Message#moco_message.forwarded_by end, Messages),
	AttachedList = lists:map(fun(Message) -> moco_proto:encode(attached, Message#moco_message.attached) end, Messages),
	PostedList = lists:map(fun(Message) -> Message#moco_message.posted end, Messages),
	OriginList = lists:map(fun(Message) -> moco_proto:encode(origin, Message#moco_message.origin) end, Messages),
	FavoritedList  = lists:map(fun(Message) -> Message#moco_message.favorited end, Messages),
	IrtList  = lists:map(fun(Message) -> Message#moco_message.irt_reference end, Messages),
	ParamsBin = params([{long_list, id, IdList},
						 {str_list, author, AuthorList},
						 {str_list, author_id, AuthorIdList},
						 {str_list, forwarded_by, ForwardedByList},
						 {str_list, reference, ReferenceList},
						 {str_list, body, BodyList},
						 {datetime_list, posted, PostedList},
						 {int_list, origin, OriginList},
						 {int_list, attached, AttachedList},
						 {bool_list, favorited, FavoritedList},
						 {str_list, irt, IrtList}
                        ]),
    <<?PACKET_TOPIC_PAGE:8, Id:32, ParamsBin/binary>>.

ok(Id) -> 
    <<?PACKET_OK:8, Id:32>>.

ping(Id) -> 
    <<?PACKET_PING:8, Id:32>>.

pong(Id) -> 
    <<?PACKET_PONG:8, Id:32>>.

error(Id) -> 
    <<?PACKET_ERROR:8, Id:32>>.

error_message(Id, Message) when is_list(Message) -> 
    error_message(Id, list_to_binary(Message));

error_message(Id, Message) when is_binary(Message) -> 
    Length = size(Message),
    <<?PACKET_ERROR_MESSAGE:8, Id:32, Length:16, Message/binary>>.

check_request(Checks) ->
	Params = lists:map(fun({Type, Name}) ->
							   case Type of
								   system ->
									   {int, Name, ?TEST_SYSTEM_PROPERTY};
								   midlet ->
									   {int, Name, ?TEST_MIDLET_PROPERTY};
								   platform ->
									   {int, Name, ?TEST_PLATFORM_PROPERTY};
								   permission ->
									   {int, Name, ?TEST_PERMISSION};
								   class ->
									   {int, Name, ?TEST_CLASS};
								   memory ->
									   {int, Name, ?TEST_MEMORY};
								   lwuit ->
									   {int, Name, ?TEST_LWUIT};
								   platform_req ->
									   {int, Name, ?TEST_PLATFORM_REQUEST};
								   platform_req_fatal ->
									   {int, Name, ?TEST_PLATFORM_REQUEST_FATAL};
								   dialog_info ->
									   {int, Name, ?TEST_DIALOG_INFO};
								   dialog_confirm ->
									   {int, Name, ?TEST_DIALOG_CONFIRM}
							   end
					   end, Checks),
    iolist_to_binary([<<?PACKET_CHECK_REQUEST:8, 0:32>>, params(Params)]).

settings(Settings) ->
    iolist_to_binary([<<?PACKET_SETTINGS:8, 0:32>>, params(Settings)]).

params(Id, Params) ->
    iolist_to_binary([<<?PACKET_PARAMS:8, Id:32>>, params(Params)]).

params(Params) ->
    IoList = lists:foldr(fun({Type, Key, Value}, Acc) ->
                            KeyBinary = param_key(Key),
                            [<<(size(KeyBinary)):16, KeyBinary/binary, (param(Type, Value))/binary>> | Acc]
                end, [], Params),
    iolist_to_binary([<<(length(Params)):8>>, IoList]).

param_key(Key) when is_list(Key) ->
    list_to_binary(Key);

param_key(Key) when is_atom(Key) ->
    list_to_binary(atom_to_list(Key)).

param(_, undefined) ->
    <<?PARAM_UNDEFINED:8>>;

param(_, null) ->
    <<?PARAM_UNDEFINED:8>>;

param(bin, Value) when is_binary(Value) ->
    <<?PARAM_BIN:8, (size(Value)):32, Value/binary>>;

param(img, Value) when is_binary(Value) ->
    <<?PARAM_IMG:8, (size(Value)):32, Value/binary>>;

param(img, Value) when is_list(Value) ->
    BinaryValue = iolist_to_binary(Value),
    <<?PARAM_IMG:8, (size(BinaryValue)):32, BinaryValue/binary>>;

param(str, Value) when is_binary(Value) ->
    <<?PARAM_STR:8, (size(Value)):16, Value/binary>>;

param(str, Value) when is_list(Value) ->
    ValueBinary =  unicode:characters_to_binary(Value),
    <<?PARAM_STR:8, (size(ValueBinary)):16, ValueBinary/binary>>;

param(str, Value) when is_atom(Value) ->
    ValueBinary =  list_to_binary(atom_to_list(Value)),
    <<?PARAM_STR:8, (size(ValueBinary)):16, ValueBinary/binary>>;

param(str, Value) when is_integer(Value) ->
    ValueBinary =  list_to_binary(integer_to_list(Value)),
    <<?PARAM_STR:8, (size(ValueBinary)):16, ValueBinary/binary>>;

param(int, Value) when is_integer(Value) ->
    <<?PARAM_INT:8, Value:32>>;

param(long, Value) when is_integer(Value) ->
    <<?PARAM_LONG:8, Value:64>>;

param(bool, true) ->
    <<?PARAM_BOOL:8, 1:8>>;

param(bool, false) ->
    <<?PARAM_BOOL:8, 0:8>>;

param(int_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    IntListBinary = iolist_to_binary(lists:map(fun(Int) -> <<Int:32>> end, Value)),
    <<?PARAM_INT_LIST:8, ListLength:32, IntListBinary/binary>>;

param(long_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    LongListBinary = iolist_to_binary(lists:map(fun(Long) -> <<Long:64>> end, Value)),
    <<?PARAM_LONG_LIST:8, ListLength:32, LongListBinary/binary>>;

param(bool_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    BoolListBinary = iolist_to_binary(lists:map(fun(1) -> <<1:8>>; (true) -> <<1:8>>; (_) -> <<0:8>> end, Value)),
    <<?PARAM_BOOL_LIST:8, ListLength:32, BoolListBinary/binary>>;

param(datetime, Value) when is_integer(Value) ->
	UnixTimestampLong = Value * 1000,
    <<?PARAM_DATETIME:8, UnixTimestampLong:64>>;

param(datetime_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    DatetimeListBinary = iolist_to_binary(lists:map(fun(T) -> TLong = T * 1000, <<TLong:64>> end, Value)),
    <<?PARAM_DATETIME_LIST:8, ListLength:32, DatetimeListBinary/binary>>;

param(str_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    StrListBinary = iolist_to_binary(lists:map(fun (undefined) -> <<0:16>>;
                                                   (null) -> <<0:16>>;
                                                   (Str) -> BinStr = moco_util:to_binary(Str), <<(size(BinStr)):16, BinStr/binary>>
                                               end, Value)),
    <<?PARAM_STR_LIST:8, ListLength:32, StrListBinary/binary>>;

param(bin_list, Value) when is_list(Value) ->
    ListLength = length(Value),
    BinListBinary = iolist_to_binary(lists:map(fun(Bin) ->
                                                case Bin of
                                                    undefined ->
                                                        <<0:32>>;
                                                    _ -> <<(size(Bin)):32, Bin/binary>>
                                                end
                                            end, Value)),
    <<?PARAM_BIN_LIST:8, ListLength:32, BinListBinary/binary>>;

param(params, Value) ->
    ParamsBin = params(Value),
    <<?PARAM_PARAMS:8, ParamsBin/binary>>.
