-module(moco_twitter).
-compile(export_all).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(TIMEOUT, 61000).

-include("moco.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channels=[], timer, oauth, secret, throttled=[]}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Secret, OAuthConsumer) ->
    gen_server:start_link(?MODULE, [Secret, OAuthConsumer], []).

stop(ServerRef) ->
    gen_server:cast(ServerRef, stop).

open(ServerRef, Channel, Client) ->
    gen_server:cast(ServerRef, {open, Channel, Client}).

read(ServerRef, Id, Client, MaxMessages) ->
    gen_server:call(ServerRef, {read, Id, Client, MaxMessages}, infinity).

poll(ServerRef, Id, Client) ->
    gen_server:cast(ServerRef, {poll, Id, Client}).

throttle(ServerRef, Client) ->
    gen_server:cast(ServerRef, {throttle, Client}).

unthrottle(ServerRef, Client) ->
    gen_server:cast(ServerRef, {unthrottle, Client}).

unfocus(ServerRef, Id) ->
    gen_server:cast(ServerRef, {unfocus, Id}).

unsubscribe(ServerRef, Client) ->
    gen_server:cast(ServerRef, {unsubscribe, Client}).

periodic(ServerRef) ->
    gen_server:cast(ServerRef, periodic).

send(ServerRef, Message, Profile, Client) ->
    gen_server:cast(ServerRef, {send, Message, Profile, Client}).

destroy_status(ServerRef, StatusId, Profile, Client) ->
    gen_server:cast(ServerRef, {destroy_status, StatusId, Profile, Client}).

destroy_dm(ServerRef, DmId, Profile, Client) ->
    gen_server:cast(ServerRef, {destroy_dm, DmId, Profile, Client}).

destroy_favorite(ServerRef, StatusId, Profile, Client) ->
    gen_server:cast(ServerRef, {destroy_favorite, StatusId, Profile, Client}).

user_show(ServerRef, Username, Profile, From) ->
	gen_server:cast(ServerRef, {user_show, Username, Profile, From}).

follow(ServerRef, Username, Profile, Client) ->
    gen_server:cast(ServerRef, {follow, Username, Profile, Client}).

unfollow(ServerRef, Username, Profile, Client) ->
    gen_server:cast(ServerRef, {unfollow, Username, Profile, Client}).

retweet(ServerRef, Reference, Profile, Client) ->
    gen_server:cast(ServerRef, {retweet, Reference, Profile, Client}).

create_favorite(ServerRef, Reference, Profile, Client) ->
    gen_server:cast(ServerRef, {create_favorite, Reference, Profile, Client}).

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
init([Secret, OAuthConsumer]) ->
    {ok, Timer} = timer:apply_after(?TIMEOUT, moco_twitter, periodic, [self()]),
    {ok, #state{secret=Secret, oauth=OAuthConsumer, channels=[], timer=timer}}.

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
handle_call({read, Id, Client, MaxMessages}, _From, #state{oauth=OAuthConsumer, channels=Channels} = State) ->
    %% read channel but don't send any updates/notifications
    {Channel, Other} = channel(Id, Channels),
    [NewChannel] = poll_many(OAuthConsumer, [Channel], false),
    NewChannels = [channel_focus(channel_add_client(Client, NewChannel)) | Other],
    Messages = (proplists:get_value(topic, NewChannel)):messages(MaxMessages),
    case {length(Messages) > 0, proplists:get_value(error, NewChannel)} of
        {true, _} ->
            Response = Messages;
        {false, undefined} ->
            Response = Messages;
        {false, Error} ->
            Response = {error, Error}
    end,
    {reply, Response, State#state{channels=NewChannels}};

handle_call(_, _, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({open, NewChannel, Client}, #state{channels=Channels} = State) ->
    io:format("open ~p ~p~n", [proplists:get_value(id, NewChannel), Client]),
    case channel(proplists:get_value(id, NewChannel), Channels) of
        {[], OtherChannels} ->
            NewChannels = [channel_unfocus(channel_add_client(Client, NewChannel)) | OtherChannels];
        {ExistingChannel, OtherChannels} ->
            NewChannels = [channel_unfocus(channel_add_client(Client, ExistingChannel)) | OtherChannels]
    end,
    {noreply, State#state{channels=NewChannels}};

handle_cast({unfocus, Id}, #state{channels=Channels} = State) ->
    {Channel, OtherChannels} = channel(Id, Channels),
    case Channel of
        [] ->
            {noreply, State};
        _ ->
            NewChannels = [channel_unfocus(Channel) | OtherChannels],
            (proplists:get_value(topic, Channel)):close(),
            {noreply, State#state{channels=NewChannels}}
    end;

handle_cast({throttle, Client}, #state{channels=Channels, throttled=Throttled} = State) ->
    NewThrottled = [Client | Throttled -- [Client]],
    NewChannels = lists:map(fun(Channel) ->
                                case proplists:get_value(clients, Channel) -- Throttled of
                                    [] ->
                                        io:format("throttling ~p~n", [proplists:get_value(id, Channel)]),
                                        [throttled | Channel -- [throttled]];
                                    _ ->
                                        Channel
                                end
                            end, Channels),
    {noreply, State#state{channels=NewChannels, throttled=NewThrottled}};

handle_cast({unthrottle, Client}, #state{channels=Channels, throttled=Throttled} = State) ->
    NewThrottled = Throttled -- [Client],
    io:format("unt ~p ~p~n", [Throttled, NewThrottled]),
    NewChannels = lists:map(fun(Channel) ->
                                io:format("checking ~p~n", [proplists:get_value(clients, Channel)]),
                                case proplists:get_value(clients, Channel) -- NewThrottled of
                                    [] ->
                                        [throttled | Channel -- [throttled]];
                                    _ ->
                                        io:format("unthrottling ~p~n", [proplists:get_value(id, Channel)]),
                                        Channel -- [throttled]
                                end
                            end, Channels),
    {noreply, State#state{channels=NewChannels, throttled=NewThrottled}};

handle_cast({unsubscribe, Client}, #state{channels=Channels} = State) ->
    NewChannels = lists:map(fun(Channel) ->
                                NewChannel = channel_del_client(Client, Channel),
                                case proplists:get_value(clients, NewChannel) of
                                    [] ->
                                        channel_unfocus(NewChannel);
                                    _ ->
                                        NewChannel
                                end
                            end, Channels),
    {noreply, State#state{channels=NewChannels}};

handle_cast({poll, Id, Client}, #state{oauth=OAuthConsumer, channels=Channels} = State) ->
    {Channel, Other} = channel(Id, Channels),
    case proplists:get_value(every, Channel) of
        never ->
            io:format("never poll channel never~n"),
            NewChannels = [channel_focus(channel_add_client(Client, Channel)) | Other];
        _ ->
            [NewChannel] = poll_many(OAuthConsumer, [Channel], true),
            NewChannels = [channel_focus(channel_add_client(Client, NewChannel)) | Other]
    end,
    {noreply, State#state{channels=NewChannels}};

handle_cast(stop, State) ->
	timer:cancel(State#state.timer),
	io:format("stop twitter~n"),
	{stop, normal, State};

handle_cast(periodic, #state{oauth=OAuthConsumer, channels=Channels} = State) ->
	{ChannelsToPoll, ChannelsToSkip} = select_channels(Channels),
	UpdatedChannels = poll_many(OAuthConsumer, ChannelsToPoll, true),
    {ok, Timer} = timer:apply_after(?TIMEOUT, moco_twitter, periodic, [self()]),
    {noreply, State#state{timer=Timer, channels=ChannelsToSkip ++ UpdatedChannels}};

handle_cast({send, #moco_message{kind=tweet, irt_reference=InReplyTo} = Message, Profile, Client}, #state{secret=Secret, oauth=OAuthConsumer, channels=Channels} = State) ->
    Args = [oauth(profile, Profile, OAuthConsumer),
            [{status, format_body(Message, Secret)}] ++ [{in_reply_to_status_id, InReplyTo} || InReplyTo /= undefined]], 
    with_retry(status_update, Args, Client,
        fun(Status, _Limits) ->
            TwitterMessage = status_to_message(Profile, Status),
            save_related(Message, TwitterMessage),    
            Ids = [{default, home}, {default, user}],
            TandC = [{proplists:get_value(topic, Channel), proplists:get_value(clients, Channel)} || Channel <- Channels, Id <- Ids, proplists:get_value(id, Channel) == Id],
            [Topic:push([TwitterMessage], Clients) || {Topic, Clients} <- TandC]
        end),
    {noreply, State};

handle_cast({send, #moco_message{kind=dm, recipient=Recipient} = Message, Profile, Client}, #state{secret=Secret, oauth=OAuthConsumer, channels=Channels} = State) ->
    Args = [oauth(profile, Profile, OAuthConsumer),
            [{text, format_body(Message, Secret)}, {user, Recipient}]],
    with_retry(direct_message_new, Args, Client,
        fun(Status, _Limits) ->
            TwitterMessage = dm_to_message(Profile, Status),
			save_related(Message, TwitterMessage),
            Ids = [{default, dm}, {default, sent}],
            TandC = [{proplists:get_value(topic, Channel), proplists:get_value(clients, Channel)} || Channel <- Channels, Id <- Ids, proplists:get_value(id, Channel) == Id],
            [Topic:push([TwitterMessage], Clients) || {Topic, Clients} <- TandC]
        end),
    {noreply, State};

handle_cast({destroy_status, StatusId, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(status_destroy, [oauth(profile, Profile, OAuthConsumer), [{id, StatusId}]], Client),
    {noreply, State};

handle_cast({destroy_dm, DmId, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(direct_message_destroy, [oauth(profile, Profile, OAuthConsumer), [{id, DmId}]], Client),
    {noreply, State};

handle_cast({destroy_favorite, StatusId, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(favorites_destroy, [oauth(profile, Profile, OAuthConsumer), [{id, StatusId}]], Client),
    {noreply, State};

handle_cast({user_show, Username, Profile, From}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(user_show, [oauth(profile, Profile, OAuthConsumer), [{id, Username}]], undefined,
        fun(User, _Limits) -> gen_server:reply(From, fix_user(User)) end,
        fun(_Reason) -> gen_server:reply(From, error) end),
    {noreply, State};

handle_cast({follow, Username, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(friendship_create, [oauth(profile, Profile, OAuthConsumer), [{id, Username}]], Client),
    {noreply, State};

handle_cast({unfollow, Username, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(friendship_destroy, [oauth(profile, Profile, OAuthConsumer), [{id, Username}]], Client),
    {noreply, State};

handle_cast({retweet, Reference, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    with_retry(status_retweet, [oauth(profile, Profile, OAuthConsumer), [{id, Reference}]], Client),
    {noreply, State};

handle_cast({create_favorite, Reference, Profile, Client}, #state{oauth=OAuthConsumer} = State) ->
    io:format("create_fav ~p~n", [Reference]),
    with_retry(favorites_create, [oauth(profile, Profile, OAuthConsumer), [{id, Reference}]], Client),
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
terminate(_Reason, #state{channels=Channels}) ->
    lists:foreach(fun(Channel) ->
                    Topic = proplists:get_value(topic, Channel),
                    Topic /= undefined andalso Topic:destroy()
                  end, Channels),
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

channel(Id, Channels) ->
    case lists:partition(fun(Channel) -> proplists:get_value(id, Channel) == Id end, Channels) of
        {[], OtherChannels} ->
            {[], OtherChannels};
        {[Channel], OtherChannels} ->
            {Channel, OtherChannels}
    end.

channel_clients(Channel) ->
    proplists:get_value(clients, Channel, []).

channel_choke(Channel, Clients) ->
    Message = #moco_message{
                origin=system,
                posted=calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) -
                       calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
                body="Too many messages, search paused."},
    (proplists:get_value(topic, Channel)):push([Message], Clients),
    [choked | Channel].

channel_update(Channel, Channels) ->
    {[_Channel], OtherChannels} = lists:partition(fun(C) -> proplists:get_value(id, C) == proplists:get_value(id, Channel) end, Channels),
    [Channel | OtherChannels].

channel_del_client(Client, Channel) ->
    proplists:delete(clients, Channel) ++ [{clients, proplists:get_value(clients, Channel) -- [Client]}].

channel_add_client(Client, Channel) ->
    proplists:delete(clients, Channel) ++ [{clients, (proplists:get_value(clients, Channel) -- [Client]) ++ [Client]}].

channel_focus(Channel) ->
    [{every, focused(proplists:get_value(id, Channel))} | proplists:delete(every, Channel)].

channel_unfocus(Channel) ->
    [{every, unfocused(proplists:get_value(id, Channel))} | proplists:delete(every, Channel)].

focused({default, home}) -> 1;
focused({default, mentions}) -> 3;
focused({default, dm}) -> 5;
focused({default, _}) -> 5;
focused({user, _}) -> 1;
focused({tweet, _}) -> never;
focused({search, _}) -> 1.

unfocused({default, home}) -> 2;
unfocused({default, mentions}) -> 2;
unfocused({default, dm}) -> 5;
unfocused({default, _}) -> never;
unfocused({user, _}) -> never;
unfocused({tweet, _}) -> never;
unfocused({search, _}) -> never.

select_channels([]) ->
	{[], []};
select_channels(Channels) ->
	{_Date, {_Hour, Minute, _Second}} = calendar:local_time(),
	lists:partition(fun(Channel) -> 
                        Period = proplists:get_value(every, Channel),
                        (Period /= never) andalso (Minute rem Period) == 0
                    end, Channels).

poll_many(OAuthConsumer, Channels, Notify) ->
    poll_many(OAuthConsumer, Channels, Notify, []).
poll_many(_OAuthConsumer, [], _Notify, Acc) ->
	Acc;
poll_many(OAuthConsumer, [Channel | Rest], Notify, Acc) ->
    UpdatedChannel = poll_one(OAuthConsumer, Channel, Notify),
	poll_many(OAuthConsumer, Rest, Notify, [UpdatedChannel | Acc]).

poll_one(OAuthConsumer, Channel, Notify) ->
    LastPollDiff = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) - 
        calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(proplists:get_value(last_poll, Channel, {0,0,0}))),
    Topic = proplists:get_value(topic, Channel),
    case {proplists:get_value(throttled, Channel, false) orelse proplists:get_value(choked, Channel, false), LastPollDiff < 60 andalso Topic:size() > 0} of
        {true, _} ->
            io:format("refusing to poll ~p, throttled~n", [proplists:get_value(id, Channel)]),
            Channel;
        {_, true} ->
            io:format("refusing to poll ~p, less than a minute from previous poll~n", [proplists:get_value(id, Channel)]),
            Channel;
        _ ->
            {Result, NewChannel} = poll_channel(Channel, OAuthConsumer),
            Clients = [Client || Client <- proplists:get_value(clients, Channel), Notify],
            case Result of
                [] -> 
                    pass;
                    %%io:format("no tweets from ~p~n", [proplists:get_value(id, Channel)]);
                [H | _ ] when is_list(H) ->
                    %%io:format("got ~p tweets from ~p~n", [length(Result), proplists:get_value(id, Channel)]),
                    Topic:push([tweet_to_message(Channel, Tweet) || Tweet <- Result], Clients);
                [H | _ ] when is_tuple(H) ->
                    %%io:format("got one tweet from ~p~n", [proplists:get_value(id, Channel)]),
                    Topic:push([tweet_to_message(Channel, Result)], Clients)
            end,
            case proplists:get_value(choke, Channel, false) of
                false ->
                    NewChannel;
                Choke ->
                    case Topic:size() > Choke of
                        true ->
                            channel_choke(NewChannel, Clients);
                        false ->
                            NewChannel
                    end
            end
    end.
            
poll_channel(Channel, OAuthConsumer) ->
    poll_channel(proplists:get_value(id, Channel), Channel, oauth(channel, Channel, OAuthConsumer)).

poll_channel({default, home}, Channel, OAuth) -> 
    poll_channel_fun(fun twerl:statuses_home_timeline/2, OAuth, Channel);

poll_channel({default, user}, Channel, OAuth) -> 
    poll_channel_fun(fun twerl:statuses_user_timeline/2, OAuth, Channel);

poll_channel({default, mentions}, Channel, OAuth) -> 
    poll_channel_fun(fun twerl:statuses_mentions/2, OAuth, Channel);

poll_channel({default, favorites}, Channel, OAuth) ->
    poll_channel_fun(fun twerl:favorites/2, OAuth, Channel);

poll_channel({default, dm}, Channel, OAuth) -> 
    {DmsSent, SentChannel} = poll_channel_fun(fun twerl:direct_messages_sent/2, OAuth, proplists:get_value(sent, Channel, moco_topic:open([{id, {default, sent}}, {profile, proplists:get_value(profile, Channel)}]))),
    {DmsReceived, ReceivedChannel} = poll_channel_fun(fun twerl:direct_messages/2, OAuth, proplists:get_value(received, Channel, moco_topic:open([{id, {default, inbox}}, {profile, proplists:get_value(profile, Channel)}]))),
    TopicSent = proplists:get_value(topic, SentChannel),
    TopicReceived = proplists:get_value(topic, ReceivedChannel),
    TopicSent:push([tweet_to_message(SentChannel, Dm) || Dm <- DmsSent], []),
    TopicReceived:push([tweet_to_message(ReceivedChannel, Dm) || Dm <- DmsReceived], []),
    Dms = lists:reverse(lists:sort(fun(StatusA, StatusB) ->
                                        A = calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, StatusA)),
                                        B = calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, StatusB)),
                                        A =< B
                                    end, DmsSent ++ DmsReceived)),
    NewChannel = [{sent, SentChannel}, {received, ReceivedChannel}] ++ proplists:delete(sent, proplists:delete(received, Channel)),
    {Dms, NewChannel};

poll_channel({user, Name}, Channel, OAuth) ->
    poll_channel_fun(fun twerl:statuses_user_timeline/2, OAuth, Channel, [{id, Name}]);

poll_channel({tweet, Reference}, Channel, OAuth) ->
    poll_channel_fun(fun twerl:statuses_show/2, OAuth, Channel, [{id, Reference}]);

poll_channel({search, Query}, Channel, _OAuth) ->
    Topic = proplists:get_value(topic, Channel),
    LastMessage = Topic /= undefined andalso Topic:last_message(),
    Args = [{q, Query}, {rpp, 100}] ++ [{since_id, LastMessage#moco_message.reference} || is_record(LastMessage, moco_message)],
    case twerl:search(Args) of
        {ok, Statuses} ->
            NewChannel = update_channel_success(Channel);
        {error, Reason} ->
            NewChannel = update_channel_failure(Channel, Reason),
            Statuses = []
    end,
    {Statuses, NewChannel}.

poll_channel_fun(TimelineFun, OAuth, Channel) ->
    poll_channel_fun(TimelineFun, OAuth, Channel, []).

poll_channel_fun(TimelineFun, OAuth, Channel, ExtraArgs) ->
    Topic = proplists:get_value(topic, Channel),
    LastMessage = Topic /= undefined andalso Topic:last_message(),
    Args = [{count, 200}] ++ [{since_id, LastMessage#moco_message.reference} || is_record(LastMessage, moco_message)]  ++ ExtraArgs,
    %%io:format("polling ~p args ~p~n", [proplists:get_value(id, Channel), Args]),
    case poll_channel_fin(TimelineFun, OAuth, Args) of
        {ok, Statuses} ->
            NewChannel = update_channel_success(Channel);
        {error, Reason} ->
            NewChannel = update_channel_failure(Channel, Reason),
            Statuses = []
    end,
    {Statuses, NewChannel}.

poll_channel_fin(TimelineFun, OAuth, Args) ->
    case apply(TimelineFun, [OAuth, Args]) of
        {ok, Statuses, _Limits} ->
                {ok, Statuses};
        {error, Reason} -> 
                io:format("error polling twitter: ~p~n", [Reason]),
                {error, Reason}
    end.

update_channel_success(Channel) ->
    [{last_poll, now()}] ++ proplists:delete(error, proplists:delete(last_poll, Channel)).

update_channel_failure(Channel, Reason) ->
    [{error, Reason}, {last_poll, now()}] ++ proplists:delete(error, proplists:delete(last_poll, Channel)). 

with_retry(Function, Args, Client) ->
    with_retry(Function, Args, Client, fun(_Result, _Limits) -> pass end).

with_retry(Function, Args, Client, FunSuccess) ->
    with_retry(Function, Args, Client, FunSuccess,
                fun(_Reason) -> Client !  {error_message, "Operation failed " ++ atom_to_list(Function)} end).

with_retry(Function, Args, Client, FunSuccess, FunFailure) ->
    with_retry(Function, Args, Client, FunSuccess, FunFailure, 3).

with_retry(Function, Args, Client, FunSuccess, FunFailure, Retries) ->
    with_retry(Function, Args, Client, FunSuccess, FunFailure, Retries, 0).

with_retry(Function, Args, Client, FunSuccess, FunFailure, Retries, Attempt) ->
    case apply(twerl, Function, Args) of
        {error, {noauth, _, _} = Reason} ->
            io:format("~p(~p) no auth: ~p giving up~n", [Function, Args, Reason]),
            FunFailure(noauth);
        {error, {message, ErrorMessage} = Reason} ->
            io:format("~p(~p) error message: ~p giving up~n", [Function, Args, ErrorMessage]),
            FunFailure(ErrorMessage);
        {error, {rate_limit, _} = Reason} ->
            io:format("~p(~p) rate limited: ~p giving up~n", [Function, Args, Reason]),
            FunFailure(Reason);
        {error, Reason} when Attempt < Retries ->
            io:format("~p(~p) failed with: ~p retrying~n", [Function, Args, Reason]),
            timer:sleep(Attempt * 1000),
            with_retry(Function, Args, Client, FunSuccess, FunFailure, Retries, Attempt + 1);
        {error, Reason} ->
            io:format("~p(~p) failed with: ~p giving up~n", [Function, Args, Reason]),
            FunFailure(Reason);
        {ok, Result, Limits} ->
            FunSuccess(Result, Limits)
    end.
    
save_related(#moco_message{attached=undefined}, _TwitterMessage) -> 
   pass;

save_related(#moco_message{attached={tweetphoto, PhotoUrl, PhotoId}}, #moco_message{reference=Reference}) -> 
    moco_db:save_related(twitter, Reference, tweetphoto, PhotoId);

save_related(#moco_message{id=Id, attached=Attached}, #moco_message{reference=Reference}) -> 
    moco_db:save_related(Id, twitter, Reference).

cache_avatar(User) ->
	UserName = proplists:get_value(screen_name, User),
	AvatarUrl = proplists:get_value(profile_image_url, User),
    moco_avatar:cache(twitter, UserName, AvatarUrl).

tweet_to_message(Channel, Tweet) ->
    tweet_to_message(proplists:get_value(id, Channel), proplists:get_value(profile, Channel), Tweet).
tweet_to_message({default, Name}, Profile, Tweet) when Name == inbox; Name == dm; Name == sent ->
    dm_to_message(Profile, Tweet);
tweet_to_message({search, _Query}, Profile, Tweet) ->
    search_result_to_message(Profile, Tweet);
tweet_to_message(_Id, Profile, Tweet) ->
    status_to_message(Profile, Tweet).

status_to_message(Profile, Status) ->
    ProfileId = proplists:get_value(profile_id, Profile),
    case proplists:get_value(retweeted_status, Status) of
        undefined ->
            cache_avatar(proplists:get_value(user, Status)),
            #moco_message{
                kind=tweet,
                reference=proplists:get_value(id, Status),
                origin=twitter,
                profile=ProfileId,
                author=proplists:get_value(screen_name, proplists:get_value(user, Status)),
                author_id=proplists:get_value(id, proplists:get_value(user, Status)),
                irt_reference=proplists:get_value(in_reply_to_status_id, Status),
                irt_user_id=proplists:get_value(in_reply_to_user_id, Status),
                favorited=proplists:get_value(favorited, Status),
                posted=calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, Status)) -
                       calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
                body=parse_body(proplists:get_value(text, Status))};
        RetweetedStatus ->
            cache_avatar(proplists:get_value(user, Status)),
            cache_avatar(proplists:get_value(user, RetweetedStatus)),
            #moco_message{
                kind=tweet,
                reference=proplists:get_value(id, Status),
                origin=twitter,
                profile=ProfileId,
                author=proplists:get_value(screen_name, proplists:get_value(user, RetweetedStatus)),
                author_id=proplists:get_value(id, proplists:get_value(user, RetweetedStatus)),
                forwarded_by=proplists:get_value(screen_name, proplists:get_value(user, Status)),
                irt_reference=proplists:get_value(in_reply_to_status_id, RetweetedStatus),
                irt_user_id=proplists:get_value(in_reply_to_user_id, RetweetedStatus),
                favorited=proplists:get_value(favorited, Status),
                posted=calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, RetweetedStatus)) -
                       calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
                body=parse_body(proplists:get_value(text, RetweetedStatus))}
    end.

dm_to_message(Profile, Dm) ->
    ProfileId = proplists:get_value(profile_id, Profile),
    Sender = proplists:get_value(sender, Dm),
    cache_avatar(Sender),
    #moco_message{
        kind=dm,
        reference=proplists:get_value(id, Dm),
        origin=twitter,
        profile=ProfileId,
        author=proplists:get_value(screen_name, Sender),
        author_id=proplists:get_value(id, Sender),
        posted=calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, Dm)) -
               calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
        favorited=false,
        body=parse_body(proplists:get_value(text, Dm))}.

search_result_to_message(Profile, Tweet) ->
    ProfileId = proplists:get_value(profile_id, Profile),
    moco_avatar:cache(twitter, proplists:get_value(from_user, Tweet), proplists:get_value(profile_image_url, Tweet)),
    #moco_message{
        kind=tweet,
        reference=proplists:get_value(id, Tweet),
        origin=twitter,
        profile=ProfileId,
        author=proplists:get_value(from_user, Tweet),
        author_id=proplists:get_value(from_user_id, Tweet),
        posted=calendar:datetime_to_gregorian_seconds(proplists:get_value(created_at, Tweet)) -
               calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
        favorited=false,
        body=parse_body(proplists:get_value(text, Tweet))}.

fix_user(User) ->
    case re:run(moco_util:to_list(proplists:get_value(profile_image_url, User)), "(http.+)_normal(\..+)$", [{capture, [1,2], list}]) of
        {match, [Base, Ext]} ->    
            ProfileUrl = Base ++ "_bigger" ++ Ext;
        _ ->
            ProfileUrl = moco_util:to_list(proplists:get_value(profile_image_url, User))
    end,
    [{profile_image_url, ProfileUrl} | proplists:delete(profile_image_url, User)].

parse_body(Text) ->
    unicode:characters_to_binary(lists:flatten(lists:map(fun(Chunk) ->
																 parse_html_entity(Chunk) end,
														 string:tokens(unicode:characters_to_list(Text), "&")))).
parse_html_entity("gt;" ++ Rest) -> ">" ++ Rest;
parse_html_entity("lt;" ++ Rest) -> "<" ++ Rest;
parse_html_entity("amp;" ++ Rest) -> "&" ++ Rest;
parse_html_entity(Else) -> Else.

format_body(#moco_message{body=Body, attached=undefined} = Message, _Secret) when is_binary(Body), size(Body) > 0 ->
    moco_util:to_list(Message#moco_message.body);

%% non-empty tweetphoto message
format_body(#moco_message{body=Body, attached={tweetphoto, PhotoUrl, _PhotoId}} = Message, Secret) when is_binary(Body), size(Body) > 0 ->
    moco_util:to_list(Message#moco_message.body) ++ " " ++ moco_util:to_list(PhotoUrl);

%% empty tweetphoto message
format_body(#moco_message{body=Body, attached={tweetphoto, PhotoUrl, _PhotoId}} = Message, Secret) ->
    moco_util:to_list(PhotoUrl);

%% non-empty pavome message
format_body(#moco_message{body=Body, attached=Kind} = Message, Secret) when is_binary(Body), size(Body) > 0 ->
    Url = url(Kind, Message#moco_message.id, Secret),
    moco_util:to_list(Message#moco_message.body) ++ " " ++ Url;

%% empty pavome message
format_body(#moco_message{attached=Kind} = Message, Secret) ->
    url(Kind, Message#moco_message.id, Secret);

format_body(_, _) -> "Empty Message".

url(Kind, Id, Secret) -> "http://pavo.me/" ++ binary_to_list(moco_proto:pack_token(Kind, Id, Secret)).

oauth(channel, Channel, OAuthConsumer) ->
    oauth(profile, proplists:get_value(profile, Channel), OAuthConsumer);
oauth(profile, Profile, OAuthConsumer) ->
    Token = moco_util:to_list(proplists:get_value(token, Profile)),
    TokenSecret = moco_util:to_list(proplists:get_value(secret, Profile)),
    {oauth, OAuthConsumer, Token, TokenSecret}.
