%%% Author  : anton
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(moco_db).

-compile(export_all).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("moco.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,
	     save_message2/2,
	     favorite_message/2,
	     unfavorite_message/2,
         user/2,
         create_user/0,
         profiles/2,
         profile/2,
         profile_add/2,
         profile_update/2,
         profile_delete/2,
         topic_for_profile/2,
         topic_size/1,
         topic_messages/2,
         topic_message/2,
         topic_last_message_by_origin/3,
         topic_messages_before/3,
         topic_messages_after/3,
         message/1,
         save_related/3,
         save_related/4,
         delete_by_reference/3,
         save_bt_device/1,
         save_bt_encounters/2,
		 encode_attached/1,
		 decode_attached/1
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_message2(TopicId, Message) when TopicId /= undefined ->
    gen_server:call(?MODULE, {save_message2, TopicId, Message}, ?DB_TIMEOUT).

favorite_message(ProfileId, Reference) ->
    gen_server:call(?MODULE, {favorite_message, ProfileId, Reference}, ?DB_TIMEOUT).

unfavorite_message(ProfileId, Reference) ->
    gen_server:call(?MODULE, {unfavorite_message, ProfileId, Reference}, ?DB_TIMEOUT).

topic_for_profile(ProfileId, Name) ->
    gen_server:call(?MODULE, {topic_for_profile, ProfileId, Name}, ?DB_TIMEOUT).

topic_message(TopicId, MessageId) ->
    gen_server:call(?MODULE, {topic_message, TopicId, MessageId}, ?DB_TIMEOUT).

topic_messages(TopicId, MaxMessages) ->
    gen_server:call(?MODULE, {topic_messages, TopicId, MaxMessages}, ?DB_TIMEOUT).

topic_size(TopicId) ->
    gen_server:call(?MODULE, {topic_size, TopicId}, ?DB_TIMEOUT).

topic_last_message_by_origin(TopicId, Origin, Profile) ->
    gen_server:call(?MODULE, {topic_last_message_by_origin, TopicId, Origin, Profile}, ?DB_TIMEOUT).

delete_by_reference(ProfileId, Origin, Reference) ->
    gen_server:call(?MODULE, {delete_by_reference, ProfileId, Origin, Reference}, ?DB_TIMEOUT).

message(MessageId) ->
    gen_server:call(?MODULE, {message, MessageId}, ?DB_TIMEOUT).

topic_messages_before(TopicId, MaxMessages, StartId) ->
    gen_server:call(?MODULE, {topic_messages_before, TopicId, MaxMessages, StartId}, ?DB_TIMEOUT).

topic_messages_after(TopicId, MaxMessages, StartId) ->
    gen_server:call(?MODULE, {topic_messages_after, TopicId, MaxMessages, StartId}, ?DB_TIMEOUT).

user(Origin, Reference) -> 
    gen_server:call(?MODULE, {user, Origin, Reference}, ?DB_TIMEOUT).

create_user() -> 
    gen_server:call(?MODULE, {create_user}, ?DB_TIMEOUT).

profiles(UserId, Origin) -> 
    gen_server:call(?MODULE, {profiles, UserId, Origin}, ?DB_TIMEOUT).

profile(Origin, Reference) -> 
    gen_server:call(?MODULE, {profile, Origin, Reference}, ?DB_TIMEOUT).

profile_add(UserId, Profile) -> 
    gen_server:call(?MODULE, {profile_add, UserId, Profile, proplists:get_value(origin, Profile)}, ?DB_TIMEOUT).

profile_update(UserId, Profile) -> 
    gen_server:call(?MODULE, {profile_update, UserId, Profile}, ?DB_TIMEOUT).

profile_delete(UserId, ProfileId) -> 
    gen_server:call(?MODULE, {profile_delete, UserId, ProfileId}, ?DB_TIMEOUT).

save_related(MessageId, Origin, Reference) -> 
    gen_server:call(?MODULE, {save_related, MessageId, Origin, Reference}, ?DB_TIMEOUT).

save_related(Origin1, Reference1, Origin2, Reference2) -> 
    gen_server:call(?MODULE, {save_related, Origin1, Reference1, Origin2, Reference2}, ?DB_TIMEOUT).

save_bt_device(BtDevice) -> 
    gen_server:call(?MODULE, {save_bt_device, BtDevice}, ?DB_TIMEOUT).

save_bt_encounters(BtDevice, Encounters) -> 
    gen_server:call(?MODULE, {save_bt_encounters, BtDevice, Encounters}, ?DB_TIMEOUT).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------

init([]) ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    connect(),
    prepare_statements(),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({save_message2, TopicId, Message}, _From, State) ->
    MessageParams = [TopicId,
					moco_proto:encode(origin, Message#moco_message.origin),
                   	Message#moco_message.profile,
                   	Message#moco_message.author,
                   	Message#moco_message.author_id,
                   	Message#moco_message.forwarded_by,
                   	Message#moco_message.reference,
				   	Message#moco_message.irt_reference,
				   	Message#moco_message.irt_user,
				   	Message#moco_message.irt_user_id,
				   	Message#moco_message.posted,
				   	Message#moco_message.body,
				   	encode_attached(Message#moco_message.attachment),
                    Message#moco_message.favorited],
    case sql_insert_id(insert_message, MessageParams, [canfail]) of
        undefined ->
            {reply, undefined, State};
	    MessageId ->
            {reply, Message#moco_message{id=MessageId, topic_id=TopicId}, State}
    end;

handle_call({favorite_message, ProfileId, Reference}, _From, State) ->
    sql_exec(favorite_message, [1, ProfileId, Reference]),
    {reply, ok, State};

handle_call({unfavorite_message, ProfileId, Reference}, _From, State) ->
    sql_exec(favorite_message, [0, ProfileId, Reference]),
    {reply, ok, State};

handle_call({topic_for_profile, ProfileId, Name}, _From, State) ->
	Result = sql_one(select_topic_by_profile_and_name, [ProfileId, Name]),
    {reply, Result, State};

handle_call({user, Origin, Reference}, _From, State) ->
    {data, MysqlResult} = mysql:execute(p1, select_user_by_profile, [moco_proto:encode(origin, Origin), Reference], ?DB_TIMEOUT),
    case mysql:get_result_rows(MysqlResult) of
        [] ->
            {reply, undefined, State};
        [[UserId]] ->
            {reply, UserId, State}
    end;

handle_call({create_user}, _From, State) ->
    {reply, sql_insert_id(insert_user), State};

handle_call({profiles, UserId, Origin}, _From, State) ->
    RawProfiles = sql(select_profiles_by_origin, [moco_proto:encode(origin, Origin), UserId]),
    Profiles = lists:map(fun decode_profile/1, RawProfiles),
    {reply, Profiles, State};

handle_call({profile, Origin, Reference}, _From, State) ->
    Profile = decode_profile(sql_row(select_profile_by_reference, [moco_proto:encode(origin, Origin), Reference])),
    {reply, Profile, State};

handle_call({profile_add, UserId, Profile, tweetphoto}, _From, State) ->
    ProfileId = sql_insert_id(insert_profile, [UserId,
                                               moco_proto:encode(origin, tweetphoto),
                                               proplists:get_value(userid, Profile),
                                               encode_profile(Profile),
                                               encode_user_profile(Profile)]),
    {reply, ProfileId, State};


handle_call({profile_add, UserId, Profile, moco}, _From, State) ->
    ProfileId = sql_insert_id(insert_profile, [UserId,
                                               moco_proto:encode(origin, moco),
                                               proplists:get_value(reference, Profile),
                                               encode_profile(Profile),
                                               encode_user_profile(Profile)]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "user"]),
    {reply, ProfileId, State};

handle_call({profile_add, UserId, Profile, twitter}, _From, State) ->
    ProfileId = sql_insert_id(insert_profile, [UserId,
                                               moco_proto:encode(origin, twitter),
                                               proplists:get_value(reference, Profile),
                                               encode_profile(Profile),
                                               encode_user_profile(Profile)]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "home"]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "user"]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "mentions"]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "inbox"]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "sent"]),
    sql_insert_id(insert_topic, [UserId, ProfileId, "dm"]),
    {reply, ProfileId, State};

handle_call({profile_update, UserId, Profile}, _From, State) ->
    sql_exec(update_profile, [encode_profile(Profile),
                              encode_user_profile(Profile),
                              UserId,
                              moco_proto:encode(origin, proplists:get_value(origin, Profile)),
                              proplists:get_value(reference, Profile)]),
    {reply, ok, State};

handle_call({topic_message, TopicId, MessageId}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_topic_message, [TopicId, MessageId], ?DB_TIMEOUT),
	[Message] = [make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    {reply, Message, State};

handle_call({topic_size, TopicId}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_topic_size, [TopicId], ?DB_TIMEOUT),
    [[TopicSize]] = mysql:get_result_rows(MysqlResult),
    {reply, TopicSize, State};

handle_call({topic_messages, TopicId, MaxMessages}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_last_topic_messages, [TopicId, MaxMessages], ?DB_TIMEOUT),
	Messages = [ make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    {reply, lists:reverse(Messages), State};

handle_call({topic_last_message_by_origin, TopicId, Orign, Profile}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_last_topic_message_by_origin, [TopicId, moco_proto:encode(origin, Orign), Profile], ?DB_TIMEOUT),
	Result = [ make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    case Result of
        [] -> 
            {reply, undefined, State};
        [Message] -> 
            {reply, Message, State}
    end;

handle_call({message, MessageId}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_message, [MessageId], ?DB_TIMEOUT),
	Result = [ make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    case Result of
        [] -> 
            {reply, undefined, State};
        [Message] -> 
            {reply, Message, State}
    end;

handle_call({topic_messages_before, TopicId, MaxMessages, StartId}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_topic_messages_before, [TopicId, StartId, MaxMessages], ?DB_TIMEOUT),
	Messages = [ make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    {reply, lists:reverse(Messages), State};

handle_call({topic_messages_after, TopicId, MaxMessages, StartId}, _From, State) ->
	{data, MysqlResult} = mysql:execute(p1, select_topic_messages_after, [TopicId, StartId, MaxMessages], ?DB_TIMEOUT),
	Messages = [ make_message(M) || M <- mysql:get_result_rows(MysqlResult)],
    {reply, Messages, State};

handle_call({save_related, MessageId, Origin, Reference}, _From, State) ->
    {updated, _MysqlResult} = mysql:execute(p1, insert_related, [MessageId, moco_proto:encode(origin, Origin), Reference], ?DB_TIMEOUT),
    {reply, ok, State};

handle_call({save_related, Origin1, Reference1, Origin2, Reference2}, _From, State) ->
    {updated, _MysqlResult} = mysql:execute(p1, insert_related2, [moco_proto:encode(origin, Origin1),
                                                                 Reference1,
                                                                 moco_proto:encode(origin, Origin2),
                                                                 Reference2], ?DB_TIMEOUT),
    {reply, ok, State};


handle_call({delete_by_reference, ProfileId, Origin, Reference}, _From, State) ->
	sql_exec(delete_message_by_reference, [ProfileId, moco_proto:encode(origin, Origin), Reference]),
	case make_message(sql_row(select_related_by_reference, [moco_proto:encode(origin, Origin), Reference])) of
		undefined ->
			ok;
		Message ->
            case Message#moco_message.attached of
                audio ->
                    moco_kv:delete([audio, amr], Message#moco_message.id), 
                    moco_kv:delete([audio, stlow], Message#moco_message.id),
                    moco_kv:delete([audio, mp4], Message#moco_message.id);
                video ->
                    moco_kv:delete([video, thumbnail], Message#moco_message.id), 
                    moco_kv:delete([video, preview], Message#moco_message.id), 
                    moco_kv:delete([video, flv], Message#moco_message.id), 
                    moco_kv:delete([video, stlow], Message#moco_message.id), 
                    moco_kv:delete([video, orig], Message#moco_message.id);
                image ->
                    moco_kv:delete([image, thumbnail], Message#moco_message.id), 
                    moco_kv:delete([image, small], Message#moco_message.id), 
                    moco_kv:delete([image, big], Message#moco_message.id), 
                    moco_kv:delete([image, orig], Message#moco_message.id);
                _ ->
                    pass
            end,
			sql_exec(delete_related, [Message#moco_message.id, moco_proto:encode(origin, Origin), Reference]),
			sql_exec(delete_message, [Message#moco_message.id])
	end,
    Related = [{moco_proto:decode(origin, Origin2), Reference2} ||
                [Origin2, Reference2] <-sql(select_related2_by_reference, [moco_proto:encode(origin, Origin), Reference])],
    sql_exec(delete_related2, [moco_proto:encode(origin, Origin), Reference]),
    {reply, Related, State};
			
handle_call({save_bt_device, BtDevice}, _From, State) ->
    Address = proplists:get_value(address, BtDevice),
    Major = proplists:get_value(major, BtDevice),
    Minor = proplists:get_value(minor, BtDevice),
    Services = proplists:get_value(services, BtDevice),
    Friendly = proplists:get_value(friendly, BtDevice),
    mysql:execute(p1, insert_bt_device, [moco_util:hex2dec(Address), Address, Major, Minor, Services, Friendly, moco_util:unix_timestamp(erlang:now())], ?DB_TIMEOUT),
    {reply, ok, State};

handle_call({save_bt_encounters, BtDevice, Encounters}, _From, State) ->
    LocalAddress = proplists:get_value(address, BtDevice),
    lists:foreach(fun({RemoteAddress, Timestamp, Major, Minor, Services, Duration, Friendly}) ->
                        mysql:execute(p1, insert_bt_device, [moco_util:hex2dec(RemoteAddress), RemoteAddress, Major, Minor, Services, Friendly, moco_util:unix_timestamp(erlang:now())], ?DB_TIMEOUT),
                        {updated, _MysqlResult} = mysql:execute(p1, insert_bt_encounter, [moco_util:hex2dec(LocalAddress), moco_util:hex2dec(RemoteAddress), Timestamp, Duration], ?DB_TIMEOUT)
                        end,
                  Encounters),
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
connect() ->
    mysql:start_link(p1, "localhost", undefined, "moco", "moco", "moco", fun(_, _, _, _) -> ok end, 'utf8').
    
prepare_statements() ->
	mysql:prepare(select_topic_by_profile_and_name, <<"SELECT topic_id FROM topic WHERE profile_id = ? AND name = ?">>),
    mysql:prepare(select_message_id_by_reference, <<"SELECT message_id FROM message WHERE origin = ? AND reference = ?">>),

    mysql:prepare(insert_message, <<"INSERT INTO message (topic_id, origin, profile, author, author_id, forwarded_by, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>),
    mysql:prepare(select_topic_size, <<"SELECT count(*) FROM message WHERE topic_id = ?">>),
    mysql:prepare(select_topic_message, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE topic_id = ? AND message_id = ?">>),
    mysql:prepare(select_last_topic_messages, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE topic_id = ? ORDER BY message_id DESC LIMIT ?">>),
    mysql:prepare(select_topic_messages_before, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE topic_id = ? AND message_id < ? ORDER BY message_id DESC LIMIT ?">>),
    mysql:prepare(select_topic_messages_after, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE topic_id = ? AND message_id > ? ORDER BY message_id ASC LIMIT ?">>),
    mysql:prepare(select_last_topic_message_by_origin, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE topic_id = ? AND origin = ? AND profile = ? ORDER BY message_id DESC LIMIT 1">>),
    mysql:prepare(select_message_by_reference, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE profile = ? AND origin = ? AND reference = ?">>),
    mysql:prepare(insert_related, <<"INSERT IGNORE INTO related (message_id, origin, reference) VALUES (?,?,?)">>),
    mysql:prepare(insert_related2, <<"INSERT INTO related2 (origin1, reference1, origin2, reference2) VALUES (?,?,?,?)">>),
    mysql:prepare(delete_related, <<"DELETE FROM related WHERE message_id = ? AND origin = ? AND reference = ?">>),
    mysql:prepare(delete_related2, <<"DELETE FROM related2 WHERE origin1 = ? AND reference1 = ?">>),
    mysql:prepare(select_related_by_reference, <<"SELECT message.message_id, topic_id, message.origin, profile, author, forwarded_by, author_id, message.reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM related JOIN message ON related.message_id = message.message_id WHERE related.origin = ? AND related.reference = ?">>),
    mysql:prepare(select_related2_by_reference, <<"SELECT origin2, reference2 FROM related2 WHERE origin1 = ? AND reference1 = ?">>),
    mysql:prepare(select_message, <<"SELECT message_id, topic_id, origin, profile, author, forwarded_by, author_id, reference, irt_reference, irt_user, irt_user_id, posted, body, attached, favorited FROM message WHERE message_id = ?">>),

    mysql:prepare(insert_user, <<"INSERT INTO user (user_id) VALUES (NULL)">>),
    mysql:prepare(insert_topic, <<"INSERT INTO topic (user_id, profile_id, name) VALUES (?,?,?)">>),
    mysql:prepare(select_profiles_by_origin, <<"SELECT user_id, profile_id, data, user_data FROM profile WHERE origin = ? AND user_id = ?">>),
    mysql:prepare(select_profile_by_reference, <<"SELECT user_id, profile_id, data, user_data FROM profile WHERE origin = ? AND reference = ?">>),
    mysql:prepare(select_user_by_profile, <<"SELECT user_id FROM profile WHERE origin = ? AND reference = ?">>),
    mysql:prepare(insert_profile, <<"INSERT INTO profile (user_id, origin, reference, data, user_data) VALUES(?,?,?,?,?)">>),
    mysql:prepare(update_profile, <<"UPDATE profile SET data = ?, user_data = ? WHERE user_id = ? AND origin = ? AND reference = ?">>),
    mysql:prepare(delete_message, <<"DELETE FROM message WHERE message_id = ?">>),
    mysql:prepare(delete_message_by_reference, <<"DELETE FROM message WHERE profile = ? AND origin = ? AND reference = ?">>),

    mysql:prepare(insert_bt_encounter, <<"INSERT INTO bt_encounter (local, remote, ts, duration) VALUES (?,?,?,?)">>),
    mysql:prepare(insert_bt_device, <<"INSERT IGNORE INTO bt_device (device_id, address, major, minor, services, friendly, ts) VALUES (?,?,?,?,?,?,?)">>),
    mysql:prepare(favorite_message, <<"UPDATE message SET favorited = ? WHERE profile = ? AND reference = ?">>).

decode_attached(0) -> undefined;
decode_attached(1) -> image;
decode_attached(2) -> video;
decode_attached(3) -> audio.

encode_attached(undefined) -> 0;
encode_attached(Attachment) ->
    case proplists:get_value(kind, Attachment) of
        image -> 1;
        video -> 2;
        audio -> 3
    end.

encode_boolean(true) -> 1;
encode_boolean(false) -> 0.

decode_boolean(0) -> false;
decode_boolean(1) -> true.

make_message(undefined) -> undefined;

make_message([Id, TopicId, Origin, Profile, Author, ForwardedBy, AuthorId, Ref, IrtRef, IrtUser, IrtUserId, Posted, Body, Attached, Favorited]) ->
    #moco_message{id=Id, topic_id=TopicId, origin=moco_proto:decode(origin, Origin), profile=Profile, author=Author, forwarded_by=ForwardedBy, author_id=AuthorId,
				  reference=Ref, irt_reference=IrtRef, irt_user=IrtUser, irt_user_id=IrtUserId, posted=Posted, body=Body,
				  attached=decode_attached(Attached), favorited=Favorited}.

encode_profile(Profile) ->
    encode_profile0(proplists:delete(user_data, Profile)).


encode_user_profile(Profile) ->
    encode_profile0(proplists:get_value(user_data, Profile, [])).

encode_profile0(Profile) ->
    Profile1 = lists:map(fun(Key) -> proplists:lookup(Key, Profile) end, lists:usort(proplists:get_keys(Profile))),
    Profile2 = lists:map(fun({Key, Value}) ->
                                case is_list(Value) of
                                    true -> {Key, list_to_binary(Value)};
                                    _ -> {Key, Value}
                                end
                            end, Profile1),
    iolist_to_binary(mochijson2:encode({struct, Profile2})).

decode_profile([UserId, ProfileId, ProfileRaw, undefined]) ->
    {struct, Plist} = mochijson2:decode(ProfileRaw),
    [{user_id, UserId}, {profile_id, ProfileId}, {user_data, []} | lists:map(fun decode_profile_element/1, Plist)];

decode_profile([UserId, ProfileId, ProfileRaw, UserDataRaw]) ->
    Profile = decode_profile([UserId, ProfileId, ProfileRaw, undefined]),
    {struct, Plist} = mochijson2:decode(UserDataRaw),
    UserData = lists:map(fun decode_profile_element/1, Plist),
    [{user_data, UserData} | proplists:delete(user_data, Profile)].

decode_profile_element({<<"origin">>, Value}) -> {origin, binary_to_atom(Value, latin1)};
decode_profile_element({Key, Value}) -> {binary_to_atom(Key, utf8), Value}.


sql(Query, Params) ->
	{data, MysqlResult} = mysql:execute(p1, Query, Params, ?DB_TIMEOUT),
    mysql:get_result_rows(MysqlResult).

sql_row(Query, Params) ->
    case sql(Query, Params) of
        [] ->
            undefined;
        [Result] ->
            Result
    end.

sql_one(Query, Params) ->
    [Result] = sql_row(Query, Params),
    Result.

sql_exec(Query, Params) ->
    case mysql:execute(p1, Query, Params, ?DB_TIMEOUT) of
        {updated, MysqlResult} ->
            MysqlResult;
        Other ->
            erang:error({exec_failed, Other})
    end.

sql_insert(Query) ->
    sql_insert(Query, [], []).

sql_insert(Query, Params) ->
    sql_insert(Query, Params, []).

sql_insert(Query, Params, Options) ->
    case mysql:execute(p1, Query, Params, ?DB_TIMEOUT) of
        {updated, MysqlResult} ->
            MysqlResult;
        Other ->
            case proplists:get_bool(canfail, Options) of
                true ->
                    undefined;
                false ->
                    erlang:error({insert_failed, Params, Options, Other})
            end
    end.

sql_insert_id(Query) ->
    sql_insert_id(Query, [], []).

sql_insert_id(Query, Param) ->
    sql_insert_id(Query, Param, []).

sql_insert_id(Query, Params, Options) ->
    case sql_insert(Query, Params, Options) of
        undefined ->
            undefined;
        MysqlResult ->
            mysql:get_result_insert_id(MysqlResult)
    end.
