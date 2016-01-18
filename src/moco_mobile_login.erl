-module(moco_mobile_login).

-include("moco.hrl").
-include("moco_mobile_packet.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
 -export([
    'WAIT_FOR_SOCKET'/2,
    'PRE_AUTH'/2,
    'CHECK_PLATFORM'/2,
    'DETECT_NOKIA'/2,
	'UPGRADE_OFFERED'/2
]).

-record(state, {socket, addr, user_id, id, params, settings, server_settings}).

-define(TIMEOUT, 600000).

-define(NOKIA_S60_SETTINGS,
        [{str, "device_photo", "capture://video"},
         {str, "device_audio", "capture://audio?encoding=amr"},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {str, "photo_snapshot_param", "encoding=jpeg&quality=80&width=640&height=480"}]).

-define(NOKIA_S40_SETTINGS,
        [{str, "video_capture_method", "canvas"},
         {bool, "url_open_die", true},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {str, "photo_capture_method", "canvas"}]).

-define(SE_SETTINGS,
        [{str, "device_photo", "capture://video"},
         {str, "device_video", "capture://audio_video"},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {bool, "video_capture_rbp", true}]).

-define(SE_C510_SETTINGS,
        [{str, "device_photo", "capture://video"},
         {str, "photo_snapshot_param", "encoding=jpeg&width=640&height=480"},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {str, "device_video", "capture://audio_video"}]).

-define(GENERIC_SETTINGS,
        [{str, "device_photo", "capture://video"},
         {str, "photo_snapshot_param", "encoding=jpeg"},
         {str, "device_video", "capture://audio_video"},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {bool, "video_capture_rbp", true}]).

-define(BTS_SETTINGS,
        [{str, "device_photo", "capture://video"},
         {str, "photo_snapshot_param", "encoding=jpeg"},
         {str, "device_video", "capture://audio_video"},
         {params, "shortcuts", [{int, "reply", $1}, {int, "retweet_new", $2}]},
         {bool, "vkb_only", true},
         {bool, "invisible_t9", false},
         {bool, "video_capture_rbp", true}]).

-define(BB_SETTINGS,
        [{params, "shortcuts", [{int, "reply", $r}, {int, "retweet_new", $l}, {int, "favorite", $v}]},
         {bool, "invisible_t9", false}]).

-define(NOKIA_S40_SETTINGS_SERVER,
	[{vendor, nokia}, {platform, s40}]).

-define(NOKIA_S60_SETTINGS_SERVER,
	[{vendor, nokia}, {platform, s60}, {rotate_video, true}]).

-define(SE_SETTINGS_SERVER,
	[{vendor, se}]).

-define(SE_C510_SETTINGS_SERVER,
	[{vendor, se}, {rotate_video, true}]).

-define(BB_SETTINGS_SERVER,
	[{vendor, blackberry}]).

-define(GENERIC_SETTINGS_SERVER,
	[{vendor, unknown}]).

-define(GENERIC_S60_SETTINGS_SERVER,
	[{vendor, unknown}, {platform, s60}]).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, {packet, 4}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'PRE_AUTH', State#state{socket=Socket, addr=IP}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(_Other, State) ->
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

'PRE_AUTH'({data, <<?PACKET_REQUEST_TOKEN:8, Id:32, ParamsBin/binary>>}, #state{socket=Socket} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    case check_username_password(proplists:get_value(username, Params), proplists:get_value(password, Params)) of
        {ok, Token} ->
            case proplists:get_value(follow, Params) of
                true -> 
                    io:format("username ok follow~n"),
                    follow_pavo_me(Token);
                _ ->
                    io:format("username ok nofollow~n"),
                    pass
            end,
            ok = gen_tcp:send(Socket, moco_mobile_packet:params(Id, [{str, token, Token}]));
        Error ->
            io:format("username check error ~p~n", [Error]),
            ok = gen_tcp:send(Socket, moco_mobile_packet:error_message(Id, "Wrong username or password"))
    end,
    {next_state, 'PRE_AUTH', State};

'PRE_AUTH'({data, <<?PACKET_LOGIN:8, Id:32, ParamsBin/binary>>}, #state{socket=Socket} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Token = proplists:get_value(token, Params),
    case moco_db:user(moco, Token) of
        undefined ->
            ok = gen_tcp:send(Socket, moco_mobile_packet:error(Id)),
	        io:format("failed to login ~p~n", [Token]),
            {next_state, 'PRE_AUTH', State};
        UserId ->
            ok = gen_tcp:send(Socket, moco_mobile_packet:check_request([{system, "video.snapshot.encodings"},
                                                                        {lwuit, "width"},
                                                                        {lwuit, "height"},
                                                                        {platform, "device_name"},
                                                                        {platform, "locale_country"},
                                                                        {memory, "free"},
                                                                        {memory, "total"}])),
            {next_state, 'CHECK_PLATFORM', State#state{user_id=UserId, id=Id, params=Params}}
    end;

'PRE_AUTH'({data, <<?PACKET_PING:8, Id:32>>}, #state{socket=Socket} = State) ->
    io:format("PING in PRE_AUTH~n"),
    ok = gen_tcp:send(Socket, moco_mobile_packet:pong(Id)),
    {next_state, 'PRE_AUTH', State};

'PRE_AUTH'({data, Data}, #state{socket=_Socket} = State) ->
    io:format("Ignoring data in PRE_AUTH: ~p~n", [Data]),
    {next_state, 'PRE_AUTH', State}.

'CHECK_PLATFORM'({data, <<?PACKET_CHECK_RESULT:8, _Id:32, ParamsBin/binary>>}, #state{params=Params} = State) ->
    Platform = moco_mobile_packet:unpack_params(ParamsBin),
    io:format("Platform ~p~n", [Platform]),
    detect(proplists:get_value(platform, Params), State#state{params=Params ++ Platform}).

'DETECT_NOKIA'({data, <<?PACKET_CHECK_RESULT:8, _Id:32, ParamsBin/binary>>}, State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    detect_nokia(State#state{params=lists:append([State#state.params, Params])}).

'UPGRADE_OFFERED'({data, <<?PACKET_CHECK_RESULT:8, _Id:32, ParamsBin/binary>>}, #state{socket=Socket, id=Id} = State) ->
    case moco_mobile_packet:unpack_params(ParamsBin) of
        [{_, true}] ->
            io:format("Upgrade~n"),
            DownloadUrl = "http://" ++ moco:property(web, host) ++ "/download.xhtml?token=" ++ moco_util:to_list(proplists:get_value(token, State#state.params)),
            ok = gen_tcp:send(Socket, moco_mobile_packet:check_request([{platform_req_fatal, DownloadUrl}])),
	        ok = gen_tcp:send(Socket, moco_mobile_packet:ok(Id)),
            timer:sleep(15000),
            {stop, normal, State};
        _ ->
            io:format("Don't upgrade~n"),
            start_session(State),
            {stop, normal, State}
    end.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, _Socket}, _StateName, #state{addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({tcp_error, Socket, Error}, _StateName, #state{socket=Socket} = StateData) ->
    error_logger:info_msg("~p Client error ~p~n", [self(), Error]),
    {stop, normal, StateData};

handle_info({error, Reason}, _StateName, StateData) ->
    error_logger:info_msg("Socket erorr ~p~n", [Reason]),
    {stop, normal, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    io:format("login shutdown~n"),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

detect(<<"NokiaE", _Rest/binary>>, State) ->
    finish(State, ?NOKIA_S60_SETTINGS, ?NOKIA_S60_SETTINGS_SERVER);

detect(<<"NokiaN", _Rest/binary>>, State) ->
    finish(State, ?NOKIA_S60_SETTINGS, ?NOKIA_S60_SETTINGS_SERVER);

detect(<<"Nokia", Rest/binary>>, #state{socket=Socket} = State) ->
    case re:run(Rest, "sw_platform=S60", [{capture, none}]) of
        match ->
            finish(State, ?NOKIA_S60_SETTINGS, ?NOKIA_S60_SETTINGS_SERVER);
        nomatch ->
            ok = gen_tcp:send(Socket, moco_mobile_packet:check_request([{system, "fileconn.dir.private"}, {system, "com.nokia.mid.timeformat"}])),
            {next_state, 'DETECT_NOKIA', State}
    end;

detect(<<"SonyEricssonC510", _Rest/binary>>, State) ->
    finish(State, ?SE_C510_SETTINGS, ?SE_C510_SETTINGS_SERVER);

detect(<<"SonyEricsson", _Rest/binary>>, #state{params=Params} = State) ->
    Encodings = string:tokens(binary_to_list(proplists:get_value('system:video.snapshot.encodings', Params, <<"">>)), " "),
    case [Encoding  || Encoding <- Encodings, string:str(Encoding, "640") /= 0] of
        [] ->
            Encoding  = "encoding=jpeg&width=480&height=640";
        [Encoding | _] ->
            Encoding
    end,
    Settings = [{str, "photo_snapshot_param", Encoding} | ?SE_SETTINGS],
    finish(State, Settings, ?SE_SETTINGS_SERVER);

detect(<<"RIM Wireless Handheld", _Rest/binary>>, State) ->
    io:format("defected blackberry~n"),
    case proplists:get_value('platform:device_name', State#state.params) of
        <<"8310">> ->
            io:format("bb light mode~n"),
            finish(State, ?BB_SETTINGS, ?BB_SETTINGS_SERVER);
        _ ->
            io:format("bb normal mode~n"),
            finish(State, ?BB_SETTINGS, ?BB_SETTINGS_SERVER)
    end;

detect(<<"BlackBerry", _Rest/binary>>, State) ->
    io:format("defected blackberry new~n"),
    case proplists:get_value('platform:device_name', State#state.params) of
        Name ->
            io:format("bb name ~p~n", [Name]),
            finish(State, ?BB_SETTINGS, ?BB_SETTINGS_SERVER)
    end;

detect(<<"SGH-F480", _Rest/binary>>, State) ->
    finish(State, ?BTS_SETTINGS, ?GENERIC_SETTINGS_SERVER);

detect(<<Other/binary>>, State) ->
    case re:run(Other, "sw_platform=S60", [{capture, none}]) of
        match ->
            finish(State, ?NOKIA_S60_SETTINGS, ?GENERIC_S60_SETTINGS_SERVER);
        nomatch ->
            io:format("sending generic settings~n"),
            finish(State, ?GENERIC_SETTINGS, ?GENERIC_SETTINGS_SERVER)
    end.

detect_nokia(State) ->
    case proplists:get_value('system:com.nokia.mid.timeformat', State#state.params) of
        undefined -> 
    		finish(State, ?NOKIA_S60_SETTINGS, ?NOKIA_S60_SETTINGS_SERVER);
        _ ->
			finish(State, ?NOKIA_S40_SETTINGS, ?NOKIA_S40_SETTINGS_SERVER)
    end.
 
finish(#state{socket=Socket} = State, Settings, ServerSettings) ->
	%% Update ServerSettings with pavo version
	NewServerSettings = [pavo_version(State) | ServerSettings],
    CurrentVersion = moco:property(midlet, version),
    AlphaVersion = moco:property(midlet, alpha_version),
	case proplists:get_value(pavo_version, NewServerSettings) of
		AlphaVersion ->
            start_session(State#state{settings=Settings, server_settings=NewServerSettings}),
    		{stop, normal, State};
		CurrentVersion ->
            start_session(State#state{settings=Settings, server_settings=NewServerSettings}),
    		{stop, normal, State};
		_ ->
    		ok = gen_tcp:send(Socket, moco_mobile_packet:check_request([{dialog_confirm, "There is a new version of PavoMe available, do you want to upgrade?"}])),
    		{next_state, 'UPGRADE_OFFERED', State#state{settings=Settings, server_settings=NewServerSettings}}
	end.

start_session(#state{socket=Socket, user_id=UserId, id=Id, settings=Settings, server_settings=ServerSettings, params=Params}) ->
    MocoProfile = hd(moco_db:profiles(UserId, moco)),
    UserData = proplists:get_value(user_data, MocoProfile, []),
    ok = gen_tcp:send(Socket, moco_mobile_packet:settings(lists:append(Settings, lists:map(fun encode_user_data_item/1, UserData)))),
    {ok, SessionPid} = supervisor:start_child(moco_mobile_session_sup, []),
    gen_tcp:controlling_process(Socket, SessionPid),
	ok = gen_tcp:send(Socket, moco_mobile_packet:ok(Id)),
    moco_log:info(session, ["started", proplists:get_value(username, MocoProfile), proplists:get_value(platform, Params), proplists:get_value(revision, Params)]),
    moco_mobile_session:set_resources(SessionPid, Socket, UserId, ServerSettings ++ Params).

pavo_version(State) ->
    {pavo_version, moco_util:to_list(proplists:get_value(revision, State#state.params))}.

encode_user_data_item({Key, Value}) when (Value == true) or (Value == false) ->
    {bool, Key, Value};

encode_user_data_item({Key, Value}) when is_integer(Value) ->
    {int, Key, Value};

encode_user_data_item({Key, Value}) when is_binary(Value) ->
    {str, Key, Value}.

download_pin_check(Pin) ->
   case ets:lookup(download_pins, Pin) of
        [{_, {PavoToken, _Timestamp}}] ->
            ets:delete(download_pins, Pin),
            PavoToken;
        _ ->
            undefined
    end.

check_username_password(Username, Password) ->
    case twerl:xauth(moco:property(twitter, oauth), binary_to_list(Username), binary_to_list(Password)) of
        {ok, Params} ->
            Reference = proplists:get_value(user_id, Params),
            case moco_db:user(twitter, Reference) of
                undefined ->
                    Token = user_create(Params ++ [{origin, twitter}, {reference, Reference}]);
                UserId ->            
                    user_update(UserId, Params ++ [{origin, twitter}, {reference, Reference}]),
                    Token = proplists:get_value(reference, hd(moco_db:profiles(UserId, moco)))
            end,
            {ok, Token};
        {error, {noauth, _Limits, Body}} ->
            {error, noauth};
        {error, Error} ->
            {error, Error}
    end.

follow_pavo_me(Id) ->
    Profile = hd(moco_db:profiles(moco_db:user(moco, Id), twitter)),
    Token = moco_util:to_list(proplists:get_value(token, Profile)),
    TokenSecret =  moco_util:to_list(proplists:get_value(secret, Profile)),
    OAuth = {oauth, moco:property(twitter, oauth),
                    moco_util:to_list(proplists:get_value(token, Profile)),
                    moco_util:to_list(proplists:get_value(secret, Profile))},
    twerl:friendship_create(OAuth, [{id, "PavoMe"}]).

user_create(TwitterProfile) ->
    io:format("undefined user~n"),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    UserId = moco_db:create_user(),
    PavoToken =  moco_proto:random_token(),
    moco_db:profile_add(UserId, TwitterProfile),
    moco_db:profile_add(UserId, [{origin, moco}, {reference, PavoToken}, {username, proplists:get_value(username, TwitterProfile)}]),
    PavoToken.

user_update(UserId, Profile) ->
    moco_db:profile_update(UserId, Profile).
