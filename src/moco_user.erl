%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(moco_user).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("moco.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1,
         session_start/2,
         session_end/2,
         throttle/1,
         unthrottle/1,
         sessions/1,
         messages/3,
         message/3,
         messages_before/4,
         messages_after/4,
         unfocus/2,
         send/2,
         delete/3,
         profiles/2,
         profile_add/2,
         user_data_update/2,
         stop/1,
		 user_info/2, 
		 follow/2,
		 unfollow/2,
		 retweet/2,
         create_favorite/2,
         destroy_favorite/2
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user_id, sessions, channels, moco_profile, twitter_profile, twitter_pid}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(UserId) -> 
    gen_server:start_link(?MODULE, [UserId], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}, infinity).

delete(Pid, What, Reference) ->
    gen_server:call(Pid, {delete, What, Reference}, infinity).

session_start(Pid, Client) ->
    gen_server:call(Pid, {session_start, Client}, infinity).

session_end(Pid, Client) ->
    gen_server:cast(Pid, {session_end, Client}).

throttle(Pid) ->
    gen_server:call(Pid, {throttle}, infinity).

unthrottle(Pid) ->
    gen_server:call(Pid, {unthrottle}, infinity).

sessions(Pid) ->
    gen_server:call(Pid, {sessions}, infinity).

messages(Pid, Id, MaxMessages) ->
    gen_server:call(Pid, {messages, Id, MaxMessages}, infinity).

message(Pid, Id, MessageId) ->
    gen_server:call(Pid, {message, Id, MessageId}, infinity).

messages_before(Pid, Id, MessageId, MaxMessages) ->
    gen_server:call(Pid, {messages_before, Id, MessageId, MaxMessages}, infinity).

messages_after(Pid, Id, MessageId, MaxMessages) ->
    gen_server:call(Pid, {messages_after, Id, MessageId, MaxMessages}, infinity).

unfocus(Pid, Id) ->
    gen_server:call(Pid, {unfocus, Id}, infinity).

profiles(Pid, Origin) ->
    gen_server:call(Pid, {profiles, Origin}, infinity).

profile_add(Pid, Profile) ->
    gen_server:call(Pid, {profile_add, Profile}, infinity).

user_data_update(Pid, UserData) ->
    gen_server:call(Pid, {user_data_update, UserData}, infinity).

user_info(Pid, Username) ->
    gen_server:call(Pid, {user_info, Username}, infinity).

follow(Pid, Username) ->
    gen_server:call(Pid, {follow, Username}, infinity).

unfollow(Pid, Username) ->
    gen_server:call(Pid, {unfollow, Username}, infinity).

retweet(Pid, Reference) ->
    gen_server:call(Pid, {retweet, Reference}, infinity).

create_favorite(Pid, Reference) ->
    gen_server:call(Pid, {create_favorite, Reference}, infinity).

destroy_favorite(Pid, Reference) ->
    gen_server:call(Pid, {destroy_favorite, Reference}, infinity).

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
init([UserId]) ->
    TwitterProfile = hd(moco_db:profiles(UserId, twitter)),
    MocoProfile = hd(moco_db:profiles(UserId, moco)),
    {ok, TwitterPid} = moco_twitter:start_link(moco:property(web, secret), moco:property(twitter, oauth)),
    {ok, #state{user_id=UserId, sessions=[], twitter_profile=TwitterProfile, twitter_pid=TwitterPid, moco_profile=MocoProfile, channels=[]}}.

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
handle_call({session_start, Client}, _From, #state{sessions=Sessions, twitter_pid=TwitterPid, channels=Channels} = State) ->

%%    lists:foreach(fun(Channel) ->
%%                    case lists:member(proplists:get_value(id, Channel), [{default, dm}, {default, mentions}]) of
%%                        true -> moco_twitter:open(TwitterPid, Channel, Client);
%%                        _ -> pass
%%                    end
%%                  end, Channels),
    {reply, ok, State#state{sessions=[Client | Sessions]}};

handle_call({sessions}, _From, State) ->
    {reply, State#state.sessions, State};

handle_call({message, Id, MessageId}, _From, #state{channels=Channels} = State) ->
    case [Channel || Channel <- Channels, proplists:get_value(id, Channel) == Id] of
        [] ->
            {reply, undefined, State};
        [Channel] ->
            Topic = proplists:get_value(topic, Channel),
            Message = Topic:message(MessageId),
            {reply, Message, State}
    end;

handle_call({messages, Id, MaxMessages}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile, channels=Channels} = State) ->
    case [Channel || Channel <- Channels, proplists:get_value(id, Channel) == Id] of
        [] ->
            io:format("new topic~n"),
            Channel = channel(Id, TwitterProfile),
            moco_twitter:open(TwitterPid, Channel, Client),
            NewChannels = [Channel | Channels];
        [Channel] ->
            io:format("existing topic~n"),
            NewChannels = Channels
    end,
    Topic = proplists:get_value(topic, Channel),
    case Topic:last_message() of
        undefined ->
            io:format("have to read~n"),
            Response = moco_twitter:read(TwitterPid, Id, Client, MaxMessages),
            {reply, Response, State#state{channels=NewChannels}};   
        _ ->
            io:format("just do poll~n"),
            moco_twitter:poll(TwitterPid, Id, Client),
            Messages = Topic:messages(MaxMessages),
            {reply, Messages, State#state{channels=NewChannels}}	
    end;

handle_call({messages_before, Id, MessageId, MaxMessages}, _From, #state{channels=Channels} = State) ->
    [Channel] = [Channel || Channel <- Channels, proplists:get_value(id, Channel) == Id],
    Messages = (proplists:get_value(topic, Channel)):messages_before(MessageId, MaxMessages),
    {reply, Messages, State};   

handle_call({messages_after, Id, MessageId, MaxMessages}, _From, #state{channels=Channels} = State) ->
    [Channel] = [Channel || Channel <- Channels, proplists:get_value(id, Channel) == Id],
    Messages = (proplists:get_value(topic, Channel)):messages_after(MessageId, MaxMessages),
    {reply, Messages, State};   

handle_call({throttle}, {Client, _Tag}, #state{twitter_pid=TwitterPid} = State) ->
    moco_twitter:throttle(TwitterPid, Client),
    {reply, ok, State};

handle_call({unthrottle}, {Client, _Tag}, #state{twitter_pid=TwitterPid} = State) ->
    moco_twitter:unthrottle(TwitterPid, Client),
    {reply, ok, State};

handle_call({unfocus, Id}, _From, #state{twitter_pid=TwitterPid} = State) ->
    io:format("unfocus name: ~p~n", [Id]),
    moco_twitter:unfocus(TwitterPid, Id),
    {reply, ok, State};   

handle_call({send, #moco_message{attached=undefined, body=Body}}, _From, State) when size(Body) == 0 ->
    %% no empty messages allowed
    {reply, ok, State};

handle_call({send, #moco_message{attached=undefined} = Message}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile, moco_profile=MocoProfile} = State) ->
    %% messages with no attachments don't go to db
    Message1 = Message#moco_message{origin=moco,
                               profile=proplists:get_value(profile_id, MocoProfile),
                               author=proplists:get_value(username, MocoProfile),
                               author_id=State#state.user_id},
    moco_twitter:send(TwitterPid, Message1, TwitterProfile, Client),
    {reply, ok, State};

handle_call({send, #moco_message{attached=Attached} = Message}, {Client, _Tag}, #state{twitter_pid=TwitterPid, moco_profile=MocoProfile, twitter_profile=TwitterProfile} = State) when Attached /= undefined ->
    Message1 = Message#moco_message{origin=moco,
                               profile=proplists:get_value(profile_id, MocoProfile),
                               author=proplists:get_value(username, MocoProfile),
                               author_id=State#state.user_id},

    case proplists:get_value(photo_service, proplists:get_value(user_data, MocoProfile), <<"tweetphoto">>) of
        <<"tweetphoto">> when Attached == image ->
            try save_attachment_tweetphoto(Message1, TwitterProfile, State#state.user_id) of
                Message2 when is_record(Message2, moco_message) ->
                    moco_twitter:send(TwitterPid, Message2, TwitterProfile, Client)
            catch
                error:_ ->
                    io:format("erorr uploading to tweetphoto, failing back to pavome~n"),
                    Message2 = save_attachment_pavome(Message1, MocoProfile),
                    moco_twitter:send(TwitterPid, Message2, TwitterProfile, Client)
            end;
        _ ->
            Message2 = save_attachment_pavome(Message1, MocoProfile),
            moco_twitter:send(TwitterPid, Message2, TwitterProfile, Client)
    end,
    {reply, ok, State};

handle_call({profiles, Origin}, _From, #state{user_id=UserId} = State) ->
    {reply, moco_db:profiles(UserId, Origin), State};

handle_call({profile_add, Profile}, _From, #state{user_id=UserId} = State) ->
    {reply, moco_db:profile_add(UserId, Profile), State};

handle_call({user_data_update, UserData}, _From, #state{user_id=UserId, moco_profile=MocoProfile} = State) ->
    MocoProfile1 = [{user_data, UserData} | proplists:delete(user_data, MocoProfile)],
    moco_db:profile_update(UserId, MocoProfile1), 
    {reply, ok, State#state{moco_profile=MocoProfile1}};

handle_call({delete, What, Reference}, {Client, _Tag}, #state{twitter_profile=TwitterProfile} = State) ->
    case What of
        tweet ->
            moco_twitter:destroy_status(State#state.twitter_pid, Reference, TwitterProfile, Client),
	        Referred = moco_db:delete_by_reference(proplists:get_value(profile_id, State#state.twitter_profile), twitter, Reference);
        dm ->
            moco_twitter:destroy_dm(State#state.twitter_pid, Reference, TwitterProfile, Client),
	        Referred = moco_db:delete_by_reference(proplists:get_value(profile_id, State#state.twitter_profile), twitter, Reference)
    end,
    lists:foreach(fun({Origin, Reference2}) ->
                      case Origin of
                          tweetphoto ->
                              [Profile] = moco_db:profiles(State#state.user_id, tweetphoto),
                              moco_tweetphoto:delete(Reference2, Profile)
                      end
                  end, Referred),
    {reply, ok, State};

handle_call({user_info, Username}, From, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
	moco_twitter:user_show(TwitterPid, Username, TwitterProfile, From),
	{noreply, State};

handle_call({follow, Username}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
	moco_twitter:follow(TwitterPid, Username, TwitterProfile, Client),
    {reply, ok, State};

handle_call({unfollow, Username}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
	moco_twitter:unfollow(TwitterPid, Username, TwitterProfile, Client),
    {reply, ok, State};

handle_call({retweet, Reference}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
	moco_twitter:retweet(TwitterPid, Reference, TwitterProfile, Client),
    {reply, ok, State};

handle_call({create_favorite, Reference}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
	moco_twitter:create_favorite(TwitterPid, Reference, TwitterProfile, Client),
    moco_db:favorite_message(proplists:get_value(profile_id, TwitterProfile), Reference),
    {reply, ok, State};

handle_call({destroy_favorite, Reference}, {Client, _Tag}, #state{twitter_pid=TwitterPid, twitter_profile=TwitterProfile} = State) ->
    io:format("destr fav~n"),
    moco_twitter:destroy_favorite(TwitterPid, Reference, TwitterProfile, Client),
    moco_db:unfavorite_message(proplists:get_value(profile_id, TwitterProfile), Reference),
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({session_end, Client}, #state{twitter_pid=TwitterPid} = State) ->
    moco_twitter:unsubscribe(TwitterPid, Client),
    case lists:delete(Client, State#state.sessions) of
        [] ->
            timer:apply_after(15000, gen_server, cast, [self(), exit_if_no_sessions]),
            {noreply, State#state{sessions=[]}};
        Sessions ->
            {noreply, State#state{sessions=Sessions}}
    end;

handle_cast(exit_if_no_sessions, State) ->
    case State#state.sessions of
        [] ->
            io:format("no active sessions left, request termination~n"),
            moco:stop_user(State#state.user_id),
            {noreply, State};
        _ ->
            io:format("some sessions postpone exit~n"),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Message, State) -> {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{twitter_pid=TwitterPid}) ->
    moco_twitter:stop(TwitterPid),
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
save_attachment_tweetphoto(Message, TwitterProfile, UserId) ->
    case moco_db:profiles(UserId, tweetphoto) of
        [Profile] ->
            ok;
        [] ->
            [TwitterProfile] = moco_db:profiles(UserId, twitter),
            Token = proplists:get_value(token, TwitterProfile),
            Secret = proplists:get_value(secret, TwitterProfile),
            Profile = moco_tweetphoto:signin(Token, Secret) ++ [{token, Token}, {secret, Secret}, {origin, tweetphoto}],
            moco_db:profile_add(UserId, Profile)
    end,
    Orig = proplists:get_value(orig, Message#moco_message.attachment),
    {ok, AttachmentUrl, PhotoId} = moco_tweetphoto:upload(Orig, Message#moco_message.body, Profile),
    Message#moco_message{attached={tweetphoto, AttachmentUrl, PhotoId}}.

save_attachment_pavome(Message, MocoProfile) ->
    TopicId = moco_db:topic_for_profile(proplists:get_value(profile_id, MocoProfile), user),
    Message2 = moco_db:save_message2(TopicId, Message),
    save_message_attachment(Message2),
    Message2.

save_message_attachment(#moco_message{attached=image, id=MessageId, attachment=Attachment}) ->
    Orig = proplists:get_value(orig, Attachment),
    Thumbnail = proplists:get_value(thumbnail, Attachment),
    Small = proplists:get_value(small, Attachment),
    Big = proplists:get_value(big, Attachment),
    moco_kv:put([image, thumbnail], MessageId, Thumbnail),
    moco_kv:put([image, small], MessageId, Small),
    moco_kv:put([image, big], MessageId, Big),
    moco_kv:put([image, orig], MessageId, Orig);

save_message_attachment(#moco_message{attached=video, id=MessageId, attachment=Attachment}) ->
    Orig = proplists:get_value(orig, Attachment),
    Flv = proplists:get_value(flv, Attachment),
    StLow = proplists:get_value(stlow, Attachment),
    Preview = proplists:get_value(preview, Attachment),
    Thumbnail = proplists:get_value(thumbnail, Attachment),
    moco_kv:put_file([video, thumbnail], MessageId, Thumbnail),
    moco_kv:put_file([video, preview], MessageId, Preview),
    moco_kv:put_file([video, flv], MessageId, Flv),
    moco_kv:put_file([video, stlow], MessageId, StLow),
    moco_kv:put_file([video, orig], MessageId, Orig),
    file:delete(Orig),
    file:delete(Flv),
    file:delete(StLow),
    file:delete(Preview),
    file:delete(Thumbnail);

save_message_attachment(#moco_message{attached=audio, id=MessageId, attachment=Attachment}) ->
    AmrFile = proplists:get_value(amr, Attachment),
    Mp4File = proplists:get_value(mp4, Attachment),
    Mp4StLowFile = proplists:get_value(stlow, Attachment),
    moco_kv:put_file([audio, amr], MessageId, AmrFile),
    moco_kv:put_file([audio, mp4], MessageId, Mp4File),
    moco_kv:put_file([audio, stlow], MessageId, Mp4StLowFile),
    file:delete(AmrFile),
    file:delete(Mp4StLowFile),
    file:delete(Mp4File);

save_message_attachment(_Message) -> ok.

channel({search, _} = Id, Profile) ->
    moco_topic:open([{id, Id}, {profile, Profile}, {clients, []}, {choke, 1000}]);

channel(Id, Profile) ->
    moco_topic:open([{id, Id}, {profile, Profile}, {clients, []}]).
