-module(moco_avatar).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(PURGE_INTERVAL, 300000). %% 5 min
-define(TIMEOUT, 30000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, cache/3, get/4, purge/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cache(Origin, Id, Info) -> 
    gen_server:call(?MODULE, {cache, Origin, Id, Info}, ?TIMEOUT).

get(Origin, Username, Client, Id) -> 
    gen_server:call(?MODULE, {get, Origin, Username, Client, Id}, ?TIMEOUT).

purge() -> 
    gen_server:call(?MODULE, {purge}, ?TIMEOUT).

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
    ets:new(avatar, [named_table, public]),
	ets:new(avatarq, [named_table, public]),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {ok, _Timer} = timer:apply_after(?PURGE_INTERVAL, ?MODULE, purge, []),
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
handle_call({cache, Origin, Id, Info}, _From, State) ->
    ets:insert_new(avatar, {{Origin, string:to_lower(moco_util:to_list(Id))}, {cached, erlang:now(), Info}}),
    {reply, ok, State};

handle_call({get, Origin, Username, Client, Id}, _From, State) ->
    LowercaseUsername = string:to_lower(moco_util:to_list(Username)),
    case {moco_kv:get([avatar, Origin], LowercaseUsername), ets:lookup(avatar, {Origin, LowercaseUsername})} of
        {undefined, [{_Key, {requested, _Timestamp}}]} ->
            {reply, undefined, State};
        {undefined, [{_Key, {cached, _Timestamp, Url}}]} ->
			ets:insert(avatar, {{Origin, LowercaseUsername}, {requested, erlang:now()}}),
			request_avatar(Origin, LowercaseUsername, Client, Id, Url),
            {reply, undefined, State};
        {Avatar, _} ->
            {reply, Avatar, State}
    end;

handle_call({purge}, _From, State) ->
    purge(ets:first(avatar)),
    {ok, _Timer} = timer:apply_after(60000, ?MODULE, purge, []),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({http, {RequestId, {error, _Reason}}}, State) ->
	[{_Key, {Origin, Username, _Client, _Id}}] = ets:lookup(avatarq, RequestId),
	ets:delete(avatarq, RequestId),
    ets:delete(avatar, {Origin, Username}),
	{noreply, State};

handle_info({http, {RequestId, {{_HTTPVersion, 200, _Text}, Headers, Body}}}, State) ->
	[{_Key, {Origin, Username, Client, Id}}] = ets:lookup(avatarq, RequestId),
	ets:delete(avatarq, RequestId),
    ets:delete(avatar, {Origin, Username}),
	ContentType = proplists:get_value("content-type", Headers),
    try
        Avatar = moco_gm:resize(Body, ContentType, "image/png", 32, 32, 0),
        moco_kv:put([avatar, Origin], Username, Avatar),
        moco_mobile_session:avatar(Client, Id, Username, Origin, Avatar)
    catch
         Class:Error ->
            io:format("failed to convert avatar: ~p~p~n", [Class, Error])
    end,
	{noreply, State};

handle_info({http, {RequestId, {_HTTPVersion, Code, Text}}}, State) ->
    io:format("failed: ~p ~p~n", [Code, Text]),
	[{_Key, {Origin, Username, _Client, _Id}}] = ets:lookup(avatarq, RequestId),
	ets:delete(avatarq, RequestId),
    ets:delete(avatar, {Origin, Username}),
	{noreply, State};

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

request_avatar(twitter, Username, Client, Id, Url) ->
	{ok, RequestId} = http:request(get, {binary_to_list(Url), []}, [], [{sync, false}]),
    ets:insert(avatarq, {RequestId, {twitter, Username, Client, Id}}).

purge('$end_of_table') ->
    ok;
purge(Key) ->
    NextKey = ets:next(avatar, Key),
    case ets:lookup(avatar, Key) of
        [{_Key, {cached, Timestamp, _Info}}] ->
            EntryAge = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) -
                        calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Timestamp)),
            case EntryAge > 300 of
                true ->
                    ets:delete(avatar, Key);
                false ->
                    pass
            end;
        _ ->
            pass
    end,
    purge(NextKey).
