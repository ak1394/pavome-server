-module(moco_mobile_session).

-include("moco.hrl").
-include("moco_mobile_packet.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_resources/4]).

-compile(export_all).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
 -export([
    'WAIT_FOR_RESOURCES'/2,
    'IDLE'/2
]).

-record(state, {
                user_pid,
                pending,
                stream,
                pings,
                socket,
                settings,
                started
               }).

-define(TIMEOUT, 300000). %% 5 min
%%-define(TIMEOUT, 10000).
-define(MAX_PINGS, 12).
-define(PAGE_SIZE, 20).

-define(EPOCH, 946684800).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_resources(Pid, Socket, UserPid, Settings) ->
    gen_fsm:send_event(Pid, {resources_ready, Socket, UserPid, Settings}).

check(Pid, Checks) ->
    gen_fsm:send_event(Pid, {check, Checks}).

settings(Pid, Settings) ->
    gen_fsm:send_event(Pid, {settings, Settings}).

error_message(Pid, Message) ->
    gen_fsm:send_event(Pid, {error_message, Message}).

avatar(Pid, Id, Username, Origin, Avatar) ->
    gen_fsm:send_event(Pid, {avatar, Id, Username, Origin, Avatar}).

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
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {ok, 'WAIT_FOR_RESOURCES', #state{pending=[], stream=undefined, started=now()}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_RESOURCES'({resources_ready, Socket, UserId, Settings}, State) when is_port(Socket) ->
    UserPid = moco:start_session(UserId),
    put(userpid, UserPid),
    moco_user:session_start(UserPid, self()),
    link(UserPid),
    inet:setopts(Socket, [{active, once}, binary]),
    finish(State#state{socket=Socket, user_pid=UserPid, settings=Settings, pings=0}).

'IDLE'({data, <<?PACKET_POST:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid, settings=Settings} = State) ->
    Message = moco_mobile_packet:unpack_params(ParamsBin),
	case should_stream(Message) of
		true ->
            {AttachedFileName, IoDevice} = tmpfile(Message),
            Fun = fun() -> send_message(UserPid, Settings, [{attached_file, AttachedFileName} | Message]), {ok, [Id]} end,
            finish(State#state{stream={IoDevice, Fun}});
		false ->
			send_message(UserPid, Settings, Message),
			finish(ok, [Id], State)
	end;

'IDLE'({data, <<?PACKET_AVATAR_REQUEST:8, Id:32, Origin:32, S1:16, Username:S1/binary>>}, State) ->
    case moco_avatar:get(moco_proto:decode(origin, Origin), Username, self(), Id) of
        undefined ->
            finish(State);
        Avatar ->
	        finish(params, [Id, [{str, username, Username}, {int, origin, Origin}, {bin, avatar, Avatar}]], State)
    end;

'IDLE'({data, <<?PACKET_TOPIC_OPEN2:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    case moco_user:messages(UserPid, topic_id(Params), ?PAGE_SIZE) of
        {error, {noauth, Reason}} ->
            finish(error_message, [Id, "This person has protected their tweets."], State);
        {error, Reason} ->
            io:format("error ~p~n", [Reason]),
            finish(error_message, [Id, "Failed"], State);
        Messages ->
            io:format("page ~p~n", [length(Messages)]),
            finish(topic_page, [Id, lists:map(fun find_attachment/1, Messages)], State)
    end;

'IDLE'({data, <<?PACKET_TOPIC_PAGE_BEFORE2:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
	Messages = moco_user:messages_before(UserPid, topic_id(Params), proplists:get_value(message_id, Params), ?PAGE_SIZE),
    finish(topic_page, [Id, lists:map(fun find_attachment/1, Messages)], State);

'IDLE'({data, <<?PACKET_TOPIC_PAGE_AFTER2:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
	Messages = moco_user:messages_after(UserPid, topic_id(Params), proplists:get_value(message_id, Params), ?PAGE_SIZE),
    finish(topic_page, [Id, lists:map(fun find_attachment/1, Messages)], State);

'IDLE'({data, <<?PACKET_TOPIC_CLOSE:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
	moco_user:unfocus(UserPid, topic_id(Params)),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_DELETE2:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Reference = proplists:get_value(reference, Params),
    case topic_id(Params) of
        {default, dm} -> moco_user:delete(UserPid, dm, Reference);
        _ -> moco_user:delete(UserPid, tweet, Reference)
    end,
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_CREATE_FAVORITE:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Reference = proplists:get_value(reference, Params),
    moco_user:create_favorite(UserPid, Reference),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_DESTROY_FAVORITE:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Reference = proplists:get_value(reference, Params),
    moco_user:destroy_favorite(UserPid, Reference),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_STATUS_REQUEST2:8, Id:32>>}, #state{user_pid=UserPid} = State) ->
    TwitterProfiles = moco_user:profiles(UserPid, twitter),
    Usernames = list_from_proplist_list(TwitterProfiles, username),
    Origins = [moco_proto:encode(origin, Origin) || Origin <- list_from_proplist_list(TwitterProfiles, origin)],
    Userids = list_from_proplist_list(TwitterProfiles, reference),
    finish(params, [Id, [{str_list, username, Usernames}, {str_list, id, Userids}, {int_list, origin, Origins}]], State);

'IDLE'({data, <<?PACKET_PONG:8, _Id:32>>}, #state{pings=Pings, user_pid=UserPid} = State) ->
    io:format("Pong~n"),
    case Pings > 0 of
        true ->
            NewPings = Pings - 1,
            moco_user:unthrottle(UserPid),
            finish(State#state{pings=NewPings});
        false ->
            error_logger:error_msg("~p Bad Ping~n", [self()]),
            {stop, normal, State}
    end;

'IDLE'({data, <<?PACKET_GET_ATTACHMENT:8, Id:32, MessageId:64, Attached:32>>}, State) ->
    case get_attachment(moco_db:message(MessageId)) of
        {image, Attachment} ->
            finish(params, [Id, [{img, image, Attachment}]], State);
        {video, Attachment} ->
            finish(params, [Id, [{str, video, Attachment}]], State);
        {audio, Attachment} ->
            finish(params, [Id, [{str, audio, Attachment}]], State);
        {tweetphoto, Plist} ->
            finish(params, [Id, [{img, image, proplists:get_value(thumbnail, Plist)}]], State);
        _ ->
            finish(params, [Id, []], State)
    end;

'IDLE'({data, <<?PACKET_GET_ATTACHMENT2:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Message = moco_user:message(UserPid, topic_id(Params), proplists:get_value(message_id, Params)),
    case get_attachment(Message) of
        {image, Attachment} ->
            finish(params, [Id, [{img, image, Attachment}]], State);
        {video, Attachment} ->
            finish(params, [Id, [{str, video, Attachment}]], State);
        {audio, Attachment} ->
            finish(params, [Id, [{str, audio, Attachment}]], State);
        {tweetphoto, Plist} ->
            finish(params, [Id, [{img, image, proplists:get_value(thumbnail, Plist)},
                                 {int, id, proplists:get_value(id, Plist)},
                                 {bool, favored, proplists:get_value(favored, Plist)},
                                 {int, voted, proplists:get_value(voted, Plist)},
                                 {int, views, proplists:get_value(views, Plist)},
                                 {int, upvotes, proplists:get_value(upvotes, Plist)},
                                 {int, downvotes, proplists:get_value(downvotes, Plist)}]], State);
        _ ->
            finish(params, [Id, []], State)
    end;

'IDLE'({data, <<?PACKET_TWEETPHOTO:8, Id:32, ParamsBin/binary>>}, State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Action = proplists:get_value(action, Params),
    PhotoId = proplists:get_value(id, Params),
    case {Action, moco_user:profiles(get(userpid), tweetphoto)} of
        {?TWEETPHOTO_FAVORITE, [Profile]} ->
            moco_tweetphoto:favorite(PhotoId, Profile);
        {?TWEETPHOTO_UNFAVORITE, [Profile]} ->
            moco_tweetphoto:unfavorite(PhotoId, Profile);
        {?TWEETPHOTO_UPVOTE, [Profile]} ->
            moco_tweetphoto:upvote(PhotoId, Profile);
        {?TWEETPHOTO_DOWNVOTE, [Profile]} ->
            moco_tweetphoto:downvote(PhotoId, Profile);
        {?TWEETPHOTO_COMMENT, [Profile]} ->
            moco_tweetphoto:comment(PhotoId, proplists:get_value(text, Params), Profile);
        _ ->
            pass
    end,
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_PREVIEW_IMAGE:8, Id:32, ImageSize:32, ImageBinary:ImageSize/binary>>}, State) ->
	%% should previews be square (with empty bands on the sides? for rotation?)
	Preview = image_preview(ImageBinary),
	PreviewId = erlang:phash2(now()),
    PreviewKey = preview_key(PreviewId),
	moco_kv:put([preview, image, preview], PreviewKey, Preview),
	moco_kv:put([preview, image, orig], PreviewKey, ImageBinary),
    finish(params, [Id, [{int, preview_id, PreviewId}, {img, preview, Preview}]], State);

'IDLE'({data, <<?PACKET_PREVIEW:8, Id:32, ParamsBin/binary>>}, State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    {Filename, File} = tmpfile2(".jpg"),
    Fun = fun() ->
        PreviewFilename = image_preview_file(Filename),
        PreviewId = erlang:phash2(now()),
        PreviewKey = preview_key(PreviewId),
        moco_kv:put_file([preview, image, preview], PreviewKey, PreviewFilename),
	    moco_kv:put_file([preview, image, orig], PreviewKey, Filename),
        {ok, PreviewBinary} = file:read_file(PreviewFilename),
        ok = file:delete(Filename),
        ok = file:delete(PreviewFilename),
        {params, [Id, [{int, preview_id, PreviewId}, {img, preview, PreviewBinary}]]}
    end,
    finish(State#state{stream={File, Fun}});

'IDLE'({data, <<?PACKET_DELETE_PREVIEW:8, Id:32, PreviewId:32>>}, State) ->
    PreviewKey = integer_to_list(erlang:phash2(self())) ++ "-" ++ integer_to_list(PreviewId),
    moco_kv:delete([preview, image, orig], PreviewKey),
    moco_kv:delete([preview, image, preview], PreviewKey),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_ACK:8, Id:32, Ack:32>>}, #state{pending=Pending} = State) ->
    io:format("ACK ~p~n", [[Ack, Pending]]),
	case proplists:get_value(Ack, Pending) of
		undefined ->
    		finish(State);
		Callback ->
			Callback(Id),
			NewPending = [{Id, Callback} | proplists:delete(Ack, Pending)],
            finish(State#state{pending=NewPending})
	end;

'IDLE'({data, <<?PACKET_CHECK_RESULT:8, _Id:32, Params/binary>>}, State) ->
    MessagePlist = moco_mobile_packet:unpack_params(Params),
    io:format("check result ~p~n", [MessagePlist]),
    {next_state, 'IDLE', State, ?TIMEOUT};

'IDLE'({data, <<?PACKET_EXCEPTION:8, _Id:32, S1:16, Class:S1/binary, S2:16, Trace:S2/binary>>}, State) ->
    io:format("Exception ~p ~p~n", [Class, Trace]),
    finish(State);
    
'IDLE'({data, <<?PACKET_DELETE:8, Id:32, _Origin:32, S1:16, What:S1/binary, S2:16, Reference:S2/binary>>}, #state{user_pid=UserPid} = State) ->
    case What of
        <<"tweet">> -> moco_user:delete(UserPid, tweet, Reference);
        <<"dm">> -> moco_user:delete(UserPid, dm, Reference);
        _ -> moco_user:delete(UserPid, tweet, Reference)
    end,
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_USER_PROFILE:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    Username = proplists:get_value(username, Params),
    try moco_user:user_info(UserPid, Username) of
        error ->
            finish(error_message, [Id, "Failed to get profile of " ++ Username], State);
        UserProfile ->
            Url = proplists:get_value(profile_image_url, UserProfile),
            {ok, {_Status, Headers, Body}} = http:request(Url),
            ResizedPic = moco_gm:resize(Body, proplists:get_value("content-type", Headers), "image/png", 73, 73, 0),
            P = [{str, name, proplists:get_value(name, UserProfile)},
                 {str, username, proplists:get_value(screen_name, UserProfile)},
                 {str, url, proplists:get_value(url, UserProfile)},
                 {str, location, proplists:get_value(location, UserProfile)},
                 {str, following, proplists:get_value(following, UserProfile)},
                 {str, description, proplists:get_value(description, UserProfile)},
                 {str, followers_count, proplists:get_value(followers_count, UserProfile)},
                 {str, friends_count, proplists:get_value(friends_count, UserProfile)},
                 {str, statuses_count, proplists:get_value(statuses_count, UserProfile)},
                 {str, id, moco_util:to_list(proplists:get_value(id, UserProfile))},
                 {img, image, ResizedPic}],
            finish(params, [Id, P], State)
    catch
        A:B ->
            io:format("exception ~p~n", [[A, B]]),
            finish(error_message, [Id, "Failed to get profile of " ++ Username], State)
    end;


'IDLE'({data, <<?PACKET_FOLLOW:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Username = proplists:get_value(username, moco_mobile_packet:unpack_params(ParamsBin)),
    moco_user:follow(UserPid, Username),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_UNFOLLOW:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Username = proplists:get_value(username, moco_mobile_packet:unpack_params(ParamsBin)),
    moco_user:unfollow(UserPid, Username),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_RE_TWEET:8, Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Reference = proplists:get_value(reference, moco_mobile_packet:unpack_params(ParamsBin)),
    moco_user:retweet(UserPid, Reference),
    finish(ok, [Id], State);

'IDLE'({data, <<?PACKET_SETTINGS:8, _Id:32, ParamsBin/binary>>}, #state{user_pid=UserPid} = State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    moco_user:user_data_update(UserPid, Params),
    finish(State);

'IDLE'({data, <<?PACKET_REQUEST_URL:8, Id:32, ParamsBin/binary>>}, State) ->
    Params = moco_mobile_packet:unpack_params(ParamsBin),
    {ok, Document} = file:read_file("htdocs/m/test.xhtml"),
    finish(params, [Id, [{bin, document, Document}]], State);

'IDLE'({check, Checks}, State) ->
    finish(check_request, [Checks], State);

'IDLE'({settings, Settings}, State) ->
    finish(settings, [Settings], State);

'IDLE'({error_message, Message}, State) ->
    finish(error_message, [0, Message], State);

'IDLE'({avatar, Id, Username, Origin, Avatar}, State) ->
    finish(params, [Id, [{str, username, Username}, {int, origin, moco_proto:encode(origin, Origin)}, {bin, avatar, Avatar}]], State);

'IDLE'(timeout, #state{pings=Pings, user_pid=UserPid} = State) ->
    case Pings > ?MAX_PINGS of
        true ->
            error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
            {stop, normal, State};
        false ->
            io:format("Sending ping~n"),
            moco_user:throttle(UserPid),
            finish(ping, [0], State#state{pings=Pings+1})
    end;

'IDLE'({data, <<?PACKET_STREAM_CHUNK:8, Chunk/binary>>}, #state{stream={File, _Fun}} = State) ->
    file:write(File, Chunk),
    {next_state, 'IDLE', State, ?TIMEOUT};

'IDLE'({data, <<?PACKET_STREAM_END:8>>}, #state{stream={File, Fun}} = State) ->
    file:close(File),
    {FinishResult, FinishArgs} = Fun(),
    finish(FinishResult, FinishArgs, State#state{stream=[]});

'IDLE'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'IDLE', State, ?TIMEOUT}.

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
handle_info({notify, Id, Count, Hush}, 'IDLE', #state{socket=Socket, settings=Settings} = State) ->
    finish(new_message, [Id, Count, Hush], State);

handle_info({error_message, Message}, 'IDLE', #state{socket=Socket} = State) ->
    finish(error_message, [0, Message], State);

handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_error, Socket, Error}, _StateName, #state{socket=Socket} = StateData) ->
    error_logger:info_msg("~p Client ~p tcp error ~p~n", [self(), Error]),
    {stop, normal, StateData};

handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self()]),
    {stop, normal, StateData};

handle_info({'EXIT', From, Reason}, _StateName, #state{socket=Socket} = StateData) ->
    io:format("exiting because linked process died ~p ~p~n", [From, Reason]),
    {stop, normal, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(Reason, _StateName, #state{started=Started} = State) ->
    SessionAge = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) -
                        calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Started)),
    case get(userpid) of
        undefined ->
            Username = "unknown";
        UserPid ->        
            moco_user:session_end(UserPid, self()),
            Username = proplists:get_value(username, hd(moco_user:profiles(UserPid, moco))),
            unlink(UserPid)
    end,
    moco_log:info(session, ["ended", Username, SessionAge]),
    (catch gen_tcp:close(State#state.socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

preview_key(PreviewId) when is_integer(PreviewId) ->
    integer_to_list(erlang:phash2(self())) ++ "-" ++ integer_to_list(PreviewId).

image_preview(ImageBinary) ->
	moco_gm:montage(ImageBinary, "image/jpeg", "image/jpeg", 160, 160).

image_preview_file(ImageFilename) ->
	moco_gm:montage_file(ImageFilename, "image/jpeg", 160, 160).

audio_attachment(Props, _Settings) ->
    AudioFile = proplists:get_value(attached_file, Props),
    Mp4File = moco_ffmpeg:convert_amr_to_mp4(AudioFile),
    Mp4StLowFile = moco_ffmpeg:convert_amr_to_mp4_hinted_low(AudioFile),
    [{kind, audio}, {amr, AudioFile}, {mp4, Mp4File}, {stlow, Mp4StLowFile}].

image_attachment(Props, _Settings) ->
    PreviewId = proplists:get_value(preview_id, Props),
    Rotation = proplists:get_value(rotation, Props, 0),
    PreviewKey = integer_to_list(erlang:phash2(self())) ++ "-" ++ integer_to_list(PreviewId),
    Orig = moco_gm:rotate(moco_kv:get([preview, image, orig], PreviewKey), "image/jpeg", "image/jpeg", Rotation),
    Thumbnail = moco_gm:resize(Orig, "image/png", "image/jpeg", 80, 80, 0),
    Small = moco_gm:resize(Orig, "image/png", "image/jpeg", 160, 160, 0),
    Big = moco_gm:resize(Orig, "image/png", "image/jpeg", 450, undefined, 0),
    moco_kv:delete([preview, image, orig], PreviewKey),
    moco_kv:delete([preview, image, preview], PreviewKey),
    [{kind, image}, {orig, Orig}, {thumbnail, Thumbnail}, {small, Small}, {big, Big}].

video_attachment(Props, Settings) ->
    VideoFile = proplists:get_value(attached_file, Props),
    case proplists:get_value(rotate_video, Settings) of
        true ->
            FlvFile = moco_ffmpeg:convert_3gp_to_flv_rotated(VideoFile),
            PreviewFile = moco_ffmpeg:convert_3gp_to_jpg_rotated(VideoFile, "160x120"),
            ThumbnailFile = moco_ffmpeg:convert_3gp_to_jpg_rotated(VideoFile, "80x60");
        _ ->
            FlvFile = moco_ffmpeg:convert_3gp_to_flv(VideoFile),
            PreviewFile = moco_ffmpeg:convert_3gp_to_jpg(VideoFile, "160x120"),
            ThumbnailFile = moco_ffmpeg:convert_3gp_to_jpg(VideoFile, "80x60")
        end,
    StreamingLowFile = moco_ffmpeg:convert_3gp_to_3gp_hinted_low(VideoFile),
    [{kind, video}, {orig, VideoFile}, {flv, FlvFile}, {stlow, StreamingLowFile}, {thumbnail, ThumbnailFile}, {preview, PreviewFile}].

tmpfile(Props) ->
	case moco_util:to_existing_atom(proplists:get_value(attachment, Props)) of
		video -> tmpfile2(".3gp");
		audio -> tmpfile2(".amr")
	end.

tmpfile2(Extension) ->
    {ok, Name} = moco_util:mktemppath("/tmp", Extension),
    {ok, IoDevice} = file:open(Name, [write, raw]),
    {Name, IoDevice}.

find_attachment(Message) ->
    scan_message(Message, Message,
        [{"http://pavo.me/(\\S{24})", [{capture, [1], binary}], fun find_attachment_pavo/2},
         {"http://yfrog.com/(\\S+)", [{capture, none}], fun has_attachment_image/1},
         {"http://tweetphoto.com/(\\d+)", [{capture, none}], fun has_attachment_image/1},
         {"http://twitpic.com/(\\S+)", [{capture, none}], fun has_attachment_image/1}]).

find_attachment_pavo(Message, [Token]) ->
    case moco_proto:unpack_token(Token, moco:property(web, secret)) of
        undefined ->
            Message;
        {Kind, _Id} ->
            Message#moco_message{attached=Kind}
    end.

has_attachment_image(Message) ->
    Message#moco_message{attached=image}.

get_attachment(Message) ->
    try
        scan_message(Message, undefined,
            [{"http://pavo.me/(\\S{24})", [{capture, [1], binary}], fun get_attachment_pavo/2},
            {"http://tweetphoto.com/(\\d+)", [{capture, [1], binary}], fun get_attachment_twitphoto/2},
            {"http://yfrog.com/(\\S+)", [{capture, [1], binary}], fun get_attachment_yfrog/2},
            {"http://twitpic.com/(\\S+)", [{capture, [1], binary}], fun get_attachment_twitpic/2}])
    catch
        error:_ ->
            undefined
    end.

get_attachment_pavo(Message, [Token]) ->
    case moco_proto:unpack_token(Token, moco:property(web, secret)) of
        undefined ->
            undefined;
        {image, Id} ->
            {image, moco_kv:get([image, small], Id)};
        {audio, Id} ->
            {audio, "rtsp://stream.pavo.me/audio-low/" ++ integer_to_list(Id) ++ ".mp4"};
        {video, Id} ->
            {video, "rtsp://stream.pavo.me/video-low/" ++ integer_to_list(Id) ++ ".3gp"}
    end.

get_attachment_twitpic(Message, [Id]) ->
    get_attachment_by_url("http://twitpic.com/show/thumb/" ++ moco_util:to_list(Id)).

get_attachment_twitphoto(Message, [Id]) ->
    case moco_user:profiles(get(userpid), tweetphoto) of
        [Profile] ->
            ok;
        [] ->
            [TwitterProfile] = moco_user:profiles(get(userpid), twitter),
            Token = proplists:get_value(token, TwitterProfile),
            Secret = proplists:get_value(secret, TwitterProfile),
            Profile = moco_tweetphoto:signin(Token, Secret) ++ [{token, Token}, {secret, Secret}, {origin, tweetphoto}],
            moco_user:profile_add(get(userpid), Profile)
    end,
    {tweetphoto, moco_tweetphoto:get(Id, Profile)}.

get_attachment_yfrog(Message, [Id]) ->
    get_attachment_by_url("http://yfrog.com/" ++ moco_util:to_list(Id) ++ ".th.jpg").

get_attachment_by_url(Url) ->
    io:format("getting ~p~n", [Url]),
    case http:request(get, {Url, []}, [{timeout, 20000}], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Headers, Body}} ->
            {image, Body};
        Other ->
            io:format("failed to fetch url ~p: ~p~n", [Url, Other]),
            undefined
    end.

scan_message(Message, Nomatch, [{Re, Options, Fun} | T]) ->
    case re:run(Message#moco_message.body, Re, Options) of
        {match, Result} ->
            Fun(Message, Result);
        match ->
            Fun(Message);
        _ ->
            scan_message(Message, Nomatch, T)
    end;
scan_message(Message, Nomatch, []) ->
    Nomatch.

untransliterate(Text) ->
    Text1 = unicode:characters_to_list(Text),
	Decoded = moco_aist:decode(Text1),
	R1 = moco_gapi:detect(string:join(filter_twitter(Text1), " ")),
	R2 = moco_gapi:detect(string:join(filter_twitter(Decoded), " ")),
	io:format("detection result ~p ~p ~p ~p~n", [R1, R2, filter_twitter(Text1), filter_twitter(Decoded)]),
	Text.

filter_twitter(Text) ->
	lists:filter(fun(Word) ->
			case Word of
				"@" ++ _ -> false;
				"#" ++ _ -> false;
				"RT" -> false;
				_ -> true
			end
		end, string:tokens(Text, " ")).

%%
finish(State) ->
    {next_state, 'IDLE', State, ?TIMEOUT}.

finish(Function, Args, #state{socket = Socket} = State) ->
	ok = gen_tcp:send(Socket, apply(moco_mobile_packet, Function, Args)),
    {next_state, 'IDLE', State, ?TIMEOUT}.

should_stream(Message) ->
	case moco_util:to_existing_atom(proplists:get_value(attachment, Message)) of
		undefined -> false;
		image -> false;
		video -> true;
		audio -> true
	end.

send_message(UserPid, Settings, Props) ->
%%     Body = case proplists:get_value(username, Profile) of
%%                <<"ak1394">> ->
%%                      unicode:characters_to_binary(moco_translit:decode_twitter(moco_util:to_list(proplists:get_value(body, Props, ""))));
%%                 _ ->
%%                     proplists:get_value(body, Props, "")
%%             end,
    Body = proplists:get_value(body, Props, ""),
    case proplists:get_value(dm, Props) of
        undefined ->
                Kind = tweet,
                Recipient = undefined;
        Recipient ->
                Kind = dm
    end,
    Attached = moco_util:to_existing_atom(proplists:get_value(attachment, Props)),
    Attachment = case Attached of
                     undefined ->
                         undefined;
                     image ->
                         image_attachment(Props, Settings);
                     video ->
                         video_attachment(Props, Settings);
                     audio ->
                         audio_attachment(Props, Settings)
                 end,    
    Message = #moco_message{kind=Kind,
                            body=Body,
							recipient=Recipient,
							irt_reference=proplists:get_value(irt, Props),
                            favorited=false,
                       		attached=Attached,
                       		attachment=Attachment,
					   		posted=moco_util:unix_timestamp(erlang:now())},
    moco_user:send(UserPid, Message).

topic_id(Params) ->
    {topic_kind(proplists:get_value(kind, Params)), topic_name(proplists:get_value(name, Params))}.

topic_name(<<"home">>) -> home;
topic_name(<<"default">>) -> home;
topic_name(<<"user">>) -> user;
topic_name(<<"mentions">>) -> mentions;
topic_name(<<"inbox">>) -> inbox;
topic_name(<<"sent">>) -> sent;
topic_name(<<"dm">>) -> dm;
topic_name(<<"favorites">>) -> favorites;
topic_name(Other) -> Other.

topic_kind(<<"search">>) -> search;
topic_kind(<<"tweet">>) -> tweet;
topic_kind(<<"default">>) -> default;
topic_kind(<<"user">>) -> user.

list_from_proplist_list(ProplistList, Name) ->
    lists:map(fun(Proplist) -> proplists:get_value(Name, Proplist) end, ProplistList).
