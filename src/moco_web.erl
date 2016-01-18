%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Jun 2008
%%% -------------------------------------------------------------------
-module(moco_web).

-compile(export_all).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
-include_lib("clickatell.hrl").
-include("moco.hrl").

-define(DOCROOT, "htdocs").
-define(DOCROOT_M, "htdocs/m").
-define(DTLROOT, "htdocs/templates/").
-define(COOKIE, "tkt").
-define(PIN_PURGE_INTERVAL, 60000).
-define(PIN_LIFETIME, 300).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, r/0, send_sms/2]).

-record(state, {timer}).
-record(r, {token, timestamp}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
	ets:new(twitter_tokens, [named_table, public]),
	ets:new(download_pins, [named_table, public]),
    compile_dtl_templates(),
    Timer = timer:apply_interval(?PIN_PURGE_INTERVAL, moco_web, download_pin_purge, []),
	Loop = fun(Req) ->
				   io:format("req ~p ~p ~p ~p~n", [Req:get(method), Req:get(path), Req:get_header_value("referer"), Req:get_header_value("user-agent")]),
				   dispatch(Req:get_header_value("host"), Req:get(path), Req:get(method), Req) end,				   
    mochiweb_http:start([{port, 8080}, {loop, Loop}]),
   {ok, #state{timer=Timer}}.

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
handle_call(_, _From, State) ->
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
dispatch(Host, Path, Method, Req) ->
    case string:equal(string:to_lower(Host), moco:property(web, mhost)) of
        true ->
            dispatch_m(Path, Method, Req);
        _ ->
            dispatch(Path, Method, Req)
    end.

dispatch_m("/", 'GET', Req) -> Req:serve_file("index.xhtml", ?DOCROOT_M);
dispatch_m("/alpha", 'GET', Req) -> Req:serve_file("alpha.xhtml", ?DOCROOT_M);
dispatch_m("/pin", 'GET', Req) -> Req:serve_file("pin.xhtml", ?DOCROOT_M);
dispatch_m("/favicon.ico", 'GET', Req) -> Req:serve_file("static/image/favicon.ico", ?DOCROOT);
dispatch_m(Path, Method, Req) -> io:format("not found ~p ~p~n", [Method, Path]), Req:not_found().

dispatch("/", 'GET', Req) -> Req:serve_file("index.html", ?DOCROOT);
dispatch("/invite353", 'GET', Req) -> Req:serve_file("index.html", ?DOCROOT);
dispatch("/home", 'GET', Req) -> authenticated(fun(_, R) ->
                                                    {ok, Html} = tmpl_home:render([]),
                                                    R:respond({200, [{"Content-Type", "text/html"}],  Html})
                                                end, Req);
dispatch("/welcome", 'GET', Req) -> authenticated(fun(_, R) ->
                                                    {ok, Html} = tmpl_welcome:render([]),
                                                    R:respond({200, [{"Content-Type", "text/html"}],  Html})
                                                end, Req);
dispatch("/signin", 'GET', Req) -> signin(Req);
dispatch("/signout", 'GET', Req) -> signout(Req);
dispatch("/download.xhtml", 'GET', Req) -> wap(Req);
dispatch("/download/pavo.jad", 'GET', Req) -> download_jad(Req);
dispatch("/download/pavo.jar", 'GET', Req) -> download_jar(Req);
dispatch("/download/pavo-a.jad", 'GET', Req) -> download2_jad(Req);
dispatch("/download/pavo-a.jar", 'GET', Req) -> download2_jar(Req);
dispatch("/api/twitter/request_token", 'GET', Req) -> request_twitter_token(Req);
dispatch("/api/jad_token", 'GET', Req) -> authenticated(fun request_jad_token/2, Req);
dispatch("/api/send_jad_sms", 'GET', Req) -> authenticated(fun send_jad_sms/2, Req);
dispatch("/api/download_pin", 'GET', Req) -> authenticated(fun get_download_pin/2, Req);
dispatch("/api/follow_us", 'GET', Req) -> authenticated(fun follow_us/2, Req);
dispatch("/static/" ++ Path, 'GET', Req) -> Req:serve_file("static/" ++ Path, ?DOCROOT);
dispatch("/" ++ Path, 'GET', Req) when length(Path) == 24 -> message_redirect(Path, Req);
dispatch("/" ++ Path, 'GET', Req) when length(Path) == 25 -> message_html(Path, Req);
dispatch("/" ++ Path, 'GET', Req) when length(Path) > 25 -> message_content(Path, Req);
dispatch(Path, Method, Req) ->
	io:format("not found ~p ~p~n", [Method, Path]),
	Req:not_found().

authenticated(FunYes, FunNo, Req) ->
	case moco_cookie:check(Req:get_cookie_value(?COOKIE), moco:property(web, secret)) of
		false ->
			FunNo(#r{}, Req);
		{ok, Token, Timestamp} ->
			FunYes(#r{token=Token, timestamp=Timestamp}, Req)
	end.

authenticated(Fun, Req) ->
	authenticated(Fun,
				  fun(_, R) -> R:respond({403, [{"Content-Type", "text/html; charset=UTF-8"}], "403 Forbidden"}) end,
				  Req).

download_jad(Req) ->
    io:format("Download jad: ~p~n", [Req:parse_qs()]),
    Token = proplists:get_value("token", Req:parse_qs()),
    download_jad(Req, Token).

download_jad(Req, Token) ->
	{ok, FileInfo} = file:read_file_info([?DOCROOT, "/pavo.jar"]),
	MidletJarSize = "MIDlet-Jar-Size: " ++ integer_to_list(FileInfo#file_info.size),
    case proplists:get_value("nourl", Req:parse_qs(), false) of
        false ->
	        MidletJarUrl = "MIDlet-Jar-URL: " ++ "http://" ++ moco:property(web, host) ++ "/download/pavo.jar" ++ ["?token=" ++ moco_util:to_list(Token) || Token /= undefined];
        _ ->
	        MidletJarUrl = "MIDlet-Jar-URL: pavo.jar"
    end,
    PavoToken = ["Pavo-Token: " ++ moco_util:to_list(Token) || Token /= undefined],
	Jad = ["MIDlet-Version: " ++ moco:property(midlet, version),
		   "MIDlet-Vendor: PavoMe",
		   MidletJarUrl,
		   "MicroEdition-Configuration: CLDC-1.1",
           "MIDlet-1: PavoMe,/peacock46x48.png,me.pavo.Main",
		   "MicroEdition-Profile: MIDP-2.0",
		   "MIDlet-Name: PavoMe",
		   MidletJarSize,
		   PavoToken],
    case Req:get_header_value("user-agent") of
        "LG/" ++ _ ->    
            io:format("LG KF700 phone detected~n"),
            Jad2 = Jad ++ ["MIDlet-Touch-Support: true",
                           "UseNativeTextButtons: false",
                           "UseNativeCommands: false",
                           "LGE-MIDlet-TargetLCD-Height: 400",
                           "LGE-MIDlet-TargetLCD-Width: 240", 
                           "LGE-MIDlet-Height: 400",
                           "LGE-MIDlet-Width: 240"];
        "SAMSUNG" ++ _ ->    
            io:format("SAMSUNG phone detected~n"),
            Jad2 = Jad ++ ["MIDlet-Touch-Support: true"];
        _ ->
            Jad2 = Jad
    end,
    io:format("jad~n ~p~n", [lists:flatten(string:join(Jad2, "\r\n"))]),
	Req:respond({200, [{"Content-Type", "text/vnd.sun.j2me.app-descriptor"}, {"Cache-Control", "no-cache"}], string:join(Jad2, "\r\n")}).

download_jar(Req) ->
    io:format("Download jar: ~p~n", [Req:parse_qs()]),
    Req:serve_file("pavo.jar", ?DOCROOT, [{"Cache-Control", "no-cache"}]).

download2_jad(Req) ->
	{ok, FileInfo} = file:read_file_info([?DOCROOT, "/pavo-a.jar"]),
	MidletJarSize = "MIDlet-Jar-Size: " ++ integer_to_list(FileInfo#file_info.size),
    MidletJarUrl = "MIDlet-Jar-URL: " ++ "http://" ++ moco:property(web, host) ++ "/download/pavo-a.jar",
	Jad = ["MIDlet-Version: " ++ moco:property(midlet, alpha_version),
		   "MIDlet-Vendor: PavoMe",
		   MidletJarUrl,
		   "MicroEdition-Configuration: CLDC-1.1",
           "MIDlet-1: PavoMe-A,/peacock46x48.png,me.pavo.Main",
		   "MicroEdition-Profile: MIDP-2.0",
		   "MIDlet-Name: PavoMe-A",
		   MidletJarSize],
    case Req:get_header_value("user-agent") of
        "LG/" ++ _ ->    
            io:format("LG KF700 phone detected~n"),
            Jad2 = Jad ++ ["MIDlet-Touch-Support: true",
                           "UseNativeTextButtons: false",
                           "UseNativeCommands: false",
                           "LGE-MIDlet-TargetLCD-Height: 400",
                           "LGE-MIDlet-TargetLCD-Width: 240", 
                           "LGE-MIDlet-Height: 400",
                           "LGE-MIDlet-Width: 240"];
        "SAMSUNG" ++ _ ->    
            io:format("SAMSUNG phone detected~n"),
            Jad2 = Jad ++ ["MIDlet-Touch-Support: true"];
        _ ->
            Jad2 = Jad
    end,
    io:format("jad~n ~p~n", [string:join(Jad2, "\r\n")]),
	Req:respond({200, [{"Content-Type", "text/vnd.sun.j2me.app-descriptor"}, {"Cache-Control", "no-cache"}], string:join(Jad2, "\r\n")}).

download2_jar(Req) ->
    io:format("Download jar2~n"),
    Req:serve_file("pavo-a.jar", ?DOCROOT, [{"Cache-Control", "no-cache"}]).

wap(Req) ->
    Token = proplists:get_value("token", Req:parse_qs()),
    wap(Req, Token).

wap(Req, undefined) ->
    Req:serve_file("download-no-token.xhtml", ?DOCROOT);

wap(Req, Token) ->
    {ok, Html} = tmpl_download:render([{base, "http://" ++ moco:property(web, host)}, {token, Token}]),
    Req:respond({200, [{"Content-Type", "application/xhtml+xml"}],  Html}).

message_redirect(Path, Req) ->
    Base = "http://" ++ moco:property(web, host),
    Url = Base ++ "/" ++ Path ++ "/",
    Req:respond({302, [{"Location", Url}, {"Content-Type", "text/html; charset=UTF-8"}], ""}).

message_html(Path, Req) ->
    Token = string:substr(Path, 1, 24),
    case moco_proto:unpack_token(Token, moco:property(web, secret)) of
        undefined ->
            Req:not_found();
        {Kind, Id} ->
            case moco_db:message(Id) of
                undefined ->
                    Req:not_found();
                Message ->
                    TopicId = moco_db:topic_for_profile(Message#moco_message.profile, user),
                    MessagesAfter = lists:reverse([moco_util:message_to_proplist(M) || M <- moco_db:topic_messages_after(TopicId, 2, Message#moco_message.id)]),
                    NextMessage = case length(MessagesAfter) of
                                        0 -> undefined;
                                        _ -> lists:last(MessagesAfter)
                                      end,
                    MessagesBefore = lists:reverse([moco_util:message_to_proplist(M) || M <- moco_db:topic_messages_before(TopicId, 4 - length(MessagesAfter), Message#moco_message.id)]),
                    PreviousMessage = case length(MessagesBefore) of
                                        0 -> undefined;
                                        _ -> hd(MessagesBefore)
                                      end,
                    Params = [{current, moco_util:message_to_proplist(Message)}, {messages_before, MessagesBefore},
                              {messages_after, MessagesAfter}, {next, NextMessage}, {previous, PreviousMessage}],
                    Req:respond({200, [{"Content-Type", "text/html"}],  render_template(Kind, Params)})			
            end
    end.

message_content(Path, Req) ->
    Token = string:substr(Path, 1, 24),
    File = string:substr(Path, 25),
    case {moco_proto:unpack_token(Token, moco:property(web, secret)), File} of
        {undefined, _} -> Req:not_found();
        {{image, Id}, "/image.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([image, orig], Id)});
        {{image, Id}, "/small.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([image, small], Id)});
        {{image, Id}, "/preview-big.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([image, big], Id)});
        {{image, Id}, "/thumbnail.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([image, thumbnail], Id)});
        {{image, _Id}, "/original.html"} -> Req:serve_file("original.html", ?DOCROOT);
        {{video, Id}, "/video.flv"} -> Req:ok({"video/x-flv", moco_kv:get([video, flv], Id)});
        {{video, Id}, "/preview.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([video, preview], Id)});
        {{video, Id}, "/thumbnail.jpg"} -> Req:ok({"image/jpeg", moco_kv:get([video, thumbnail], Id)});
        {{audio, _Id}, "/thumbnail.jpg"} -> Req:serve_file("spacer.gif", ?DOCROOT);
        {{audio, Id}, "/audio.mp4"} -> Req:ok({"audio/mp4", moco_kv:get([audio, mp4], Id)});
        {{undefined, _Id}, "/thumbnail.jpg"} -> Req:serve_file("spacer.gif", ?DOCROOT)
    end.
    
request_twitter_token(Req) ->
    try
        Consumer = moco:property(twitter, oauth),
        Base = "http://" ++ moco:property(web, host),
        {ok, Request} = oauth:get("http://twitter.com/oauth/request_token",
                                    [{"oauth_callback", Base ++ "/signin"}], Consumer, "", ""),
        RequestParam = oauth_http:response_params(Request),
        "true" = proplists:get_value("oauth_callback_confirmed", RequestParam),
        Token = oauth:token(RequestParam),
        TokenSecret = oauth:token_secret(RequestParam),
        twitter_token_put(Token, TokenSecret),
        success(Req, list_to_binary(Token))
    catch
        Class:Error ->
            io:format("twitter get oauth token error: ~p~p~n", [Class, Error]),
            error(Req, "Failed to get Twitter OAuth token")
    end.

request_jad_token(R, Req) ->
    success(Req, R#r.token).

send_jad_sms(R, Req) ->
    Mobile = qp("mobile", Req:parse_qs()),
    send_sms(Mobile, binary_to_list(R#r.token)),
    success(Req).

get_download_pin(R, Req) ->
    Pin = download_pin_make(R#r.token),
    success(Req, moco_util:to_binary(Pin)).

follow_us(R, Req) ->
    Profile = hd(moco_db:profiles(moco_db:user(moco, R#r.token), twitter)),
    Token = moco_util:to_list(proplists:get_value(token, Profile)),
    TokenSecret =  moco_util:to_list(proplists:get_value(secret, Profile)),
    OAuth = {oauth, moco:property(twitter, oauth),
                    moco_util:to_list(proplists:get_value(token, Profile)),
                    moco_util:to_list(proplists:get_value(secret, Profile))},
    Rr = twerl:friendship_create(OAuth, [{id, "PavoMe"}]),
    success(Req).

send_sms(Mobile, Token) ->
    io:format("sending sms with ~p~p~n", [Mobile, Token]),
    Url = "http://pavo.me/download.xhtml?token=" ++ Token,
    clickatell:push(#sms{to=Mobile, from="pavo.me", text="PavoMe download"}, Url).

signin(Req) ->
    try get_twitter_profile(Req:parse_qs()) of
        Profile ->
            Reference = proplists:get_value(reference, Profile),
            case moco_db:user(twitter, Reference) of
                undefined ->
                    UserId = user_create(Profile),
                    Location = "/welcome";
                    
                UserId ->
                    Location = "/home",
                    user_update(UserId, Profile)
            end,
            MocoProfile = hd(moco_db:profiles(UserId, moco)),
            Cookie = moco_cookie:make(proplists:get_value(reference, MocoProfile), moco:property(web, secret)),
            Req:respond({302, [{"Location", Location},
                               {"Content-Type", "text/html; charset=UTF-8"},
                                mochiweb_cookies:cookie(?COOKIE, Cookie, [{path, "/"}])
                              ], ""})
    catch 
        Class:Error ->
            io:format("signin error: ~p ~p~n", [Class, Error]),
            Req:serve_file("must-signin.html", ?DOCROOT)
    end.

signout(Req) ->
    Req:respond({302, [{"Location", "/"},
					   {"Content-Type", "text/html; charset=UTF-8"},
					    mochiweb_cookies:cookie(?COOKIE, "expired", [{path, "/"}, {max_age, 0}])
					  ], ""}).

get_twitter_profile(QueryString) ->
    OauthToken  = qp("oauth_token", QueryString),
    OauthSecret = twitter_token_get(OauthToken),
    OauthVerifier  = qp("oauth_verifier", QueryString),
    Consumer = moco:property(twitter, oauth),
    {ok, Response} = oauth:get("http://twitter.com/oauth/access_token",
                                    [{"oauth_verifier", OauthVerifier}],
                                    Consumer, OauthToken, OauthSecret),
   	ResponseParams = oauth_http:response_params(Response),
    [{origin, twitter},
		{reference, proplists:get_value("user_id", ResponseParams)},
		{token, proplists:get_value("oauth_token", ResponseParams)},
		{secret, proplists:get_value("oauth_token_secret", ResponseParams)},
		{username, proplists:get_value("screen_name", ResponseParams)},
		{user_id, proplists:get_value("user_id", ResponseParams)}].

user_create(TwitterProfile) ->
    io:format("undefined user~n"),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    UserId = moco_db:create_user(),
    moco_db:profile_add(UserId, TwitterProfile),
    moco_db:profile_add(UserId, [{origin, moco}, {reference, moco_proto:random_token()}, {username, proplists:get_value(username, TwitterProfile)}]),
	UserId.
	
user_update(UserId, Profile) ->
	moco_db:profile_update(UserId, Profile).
	
qp(Name, QueryString) ->
    qp(Name, QueryString, undefined).

qp(Name, QueryString, Default) ->
       case proplists:get_value(Name, QueryString) of
        undefined -> Default;
        Result -> Result
    end.

twitter_token_put(Token, Secret) ->
    ets:insert(twitter_tokens, {Token, Secret}).

twitter_token_get(Token) ->
	case ets:lookup(twitter_tokens, Token) of
    	[{_, Secret}] ->
			ets:delete(twitter_tokens, Token),
			Secret;
		_ ->
			undefined
	end.

download_pin_make(PavoToken) ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    Pin = random:uniform(999999),
    ets:insert(download_pins, {Pin, {PavoToken, now()}}),
    Pin.

download_pin_purge() ->
    ets:foldl(fun({Pin, {_Token, Timestamp}}, _) ->
                    Difference = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())) - calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Timestamp)),
                    case Difference > ?PIN_LIFETIME of
                        true ->
                            ets:delete(download_pins, Pin),
                            io:format("removing stale pin ~p~n", [Pin]);
                        false ->
                            ok
                    end
              end, [], download_pins),
    ok.

download_pin_check(Pin) ->
   case ets:lookup(download_pins, Pin) of
        [{_, {PavoToken, _Timestamp}}] ->
            ets:delete(download_pins, Pin),
            PavoToken;
        _ ->
            undefined
    end.

success(Req) ->
       success(Req, <<"ok">>).

success(Req, [H | _] = Data) when is_tuple(H) ->
    Req:ok({"text/javascript",
        encode({struct, [{result, {struct, Data}}]})});

success(Req, Data) ->
    Req:ok({"text/javascript",
        encode({struct, [{result, Data}]})}).

error(Req, Error) when is_list(Error) ->
    error(Req, list_to_binary(Error));

error(Req, Error) ->
    error(Req, Error, Error).

error(Req, Error, Description) ->
    Req:ok({"text/javascript",
        encode({struct, [{error, Error}, {description, Description}]})}).

encode(O) ->
    %%Encoder = mochijson2:encoder([{handler, fun json_encode_handler/1}]),
    Encoder = mochijson2:encoder([]),
    Encoder(O).

render_template(Kind, Params) ->
    {ok, Result} = case Kind of
                        image -> tmpl_message_image:render(Params);
                        video -> tmpl_message_video:render(Params);
                        audio -> tmpl_message_audio:render(Params);
                        _ -> tmpl_message_text:render(Params)
                   end,
    Result.

compile_dtl_templates() ->
	Templates = [{"message-image.dtl", tmpl_message_image},
				 {"message-video.dtl", tmpl_message_video},
				 {"message-audio.dtl", tmpl_message_audio},
				 {"message-text.dtl", tmpl_message_text},
                 {"home.dtl", tmpl_home},
                 {"welcome.dtl", tmpl_welcome},
                 {"download.dtl", tmpl_download}],
	lists:foreach(fun({File, Name}) -> erlydtl:compile(?DTLROOT ++ File, Name) end, Templates).

r() -> compile_dtl_templates().
