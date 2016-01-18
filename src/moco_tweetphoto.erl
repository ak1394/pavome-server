-module(moco_tweetphoto).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("moco.hrl").
-define(TIMEOUT, 30000).
-define(HTTP_TIMEOUT, 20000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, upload/3, get/2, signin/2, upvote/2, downvote/2, favorite/2, unfavorite/2, comment/3, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

signin(Token, Secret) ->
    gen_server:call(?MODULE, {signin, Token, Secret}, ?TIMEOUT).

upload(Photo, Message, Profile) -> 
    gen_server:call(?MODULE, {upload, Photo, Message, Profile}, ?TIMEOUT).

get(PhotoId, Profile) ->
    gen_server:call(?MODULE, {get, PhotoId, Profile}, ?TIMEOUT).

upvote(PhotoId, Profile) ->
    gen_server:call(?MODULE, {upvote, PhotoId, Profile}, ?TIMEOUT).

downvote(PhotoId, Profile) ->
    gen_server:call(?MODULE, {downvote, PhotoId, Profile}, ?TIMEOUT).

favorite(PhotoId, Profile) ->
    gen_server:call(?MODULE, {favorite, PhotoId, Profile}, ?TIMEOUT).

unfavorite(PhotoId, Profile) ->
    gen_server:call(?MODULE, {unfavorite, PhotoId, Profile}, ?TIMEOUT).

comment(PhotoId, Comment, Profile) ->
    gen_server:call(?MODULE, {comment, PhotoId, Comment, Profile}, ?TIMEOUT).

delete(PhotoId, Profile) ->
    gen_server:call(?MODULE, {delete, PhotoId, Profile}, ?TIMEOUT).

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
    ets:new(tweetphoto, [named_table, public, bag]),
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
handle_call({signin, Token, Secret}, From, State) ->
	RequestId = tweetphoto_signin(Token, Secret),
    ets:insert(tweetphoto, {RequestId, signin, From}),
    {noreply, State};

handle_call({upload, Photo, Message, Profile}, From, State) ->
	RequestId = tweetphoto_upload(Photo, Message, Profile),
    ets:insert(tweetphoto, {RequestId, upload, From}),
    {noreply, State};

handle_call({get, PhotoId, Profile}, From, State) ->
    lists:foreach(fun(Operation) ->
                        RequestId = request(Operation, PhotoId, Profile),
                        ets:insert(tweetphoto, {RequestId, Operation, From})
                  end, [voted, favored, metadata, thumbnail, viewed]),
    {noreply, State};

handle_call({upvote, PhotoId, Profile}, From, State) ->
	RequestId = tweetphoto_upvote(PhotoId, Profile),
    ets:insert(tweetphoto, {RequestId, upvote, From}),
    {reply, ok, State};

handle_call({downvote, PhotoId, Profile}, From, State) ->
	RequestId = tweetphoto_downvote(PhotoId, Profile),
    ets:insert(tweetphoto, {RequestId, downvote, From}),
    {reply, ok, State};

handle_call({favorite, PhotoId, Profile}, From, State) ->
	RequestId = tweetphoto_favorite(PhotoId, Profile),
    ets:insert(tweetphoto, {RequestId, favorite, From}),
    {reply, ok, State};

handle_call({unfavorite, PhotoId, Profile}, From, State) ->
	RequestId = tweetphoto_unfavorite(PhotoId, Profile),
    ets:insert(tweetphoto, {RequestId, unfavorite, From}),
    {reply, ok, State};

handle_call({comment, PhotoId, Comment, Profile}, From, State) ->
	RequestId = tweetphoto_comment(PhotoId, Comment, Profile),
    ets:insert(tweetphoto, {RequestId, comment, From}),
    {reply, ok, State};


handle_call({delete, PhotoId, Profile}, From, State) ->
	RequestId = tweetphoto_delete(PhotoId, Profile),
    ets:insert(tweetphoto, {RequestId, delete, From}),
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
handle_info({http, {RequestId, {{_HTTPVersion, 200, _Text}, _Headers, Body}}}, State) ->
	[{RequestId, Operation, From}] = ets:lookup(tweetphoto, RequestId),
    ets:delete(tweetphoto, RequestId),
    case Operation of
        voted ->
            {struct, Plist} = mochijson2:decode(Body),
            Result = proplists:get_value(<<"Status">>, Plist),
            ets:insert(tweetphoto, {From, Operation, Result}),
            check_for_get_completed(From);
        favored ->        
            {struct, Plist} = mochijson2:decode(Body),
            Result = proplists:get_value(<<"IsFavorite">>, Plist),
            ets:insert(tweetphoto, {From, Operation, Result}),
            check_for_get_completed(From);
        metadata ->        
            {struct, Plist} = mochijson2:decode(Body),
            Result = [{upvotes, proplists:get_value(<<"LikedVotes">>, Plist)},
                      {downvotes, proplists:get_value(<<"UnLikedVotes">>, Plist)},
                      {id, proplists:get_value(<<"Id">>, Plist)},
                      {views, proplists:get_value(<<"Views">>, Plist)}],
            ets:insert(tweetphoto, {From, Operation, Result}),
            check_for_get_completed(From);
        thumbnail ->        
            ets:insert(tweetphoto, {From, Operation, Body}),
            check_for_get_completed(From);
        signin ->
            {struct, Plist} = mochijson2:decode(Body),
            Result = [{userid, proplists:get_value(<<"Id">>, Plist)}],
            gen_server:reply(From, Result);
        _ ->
            pass
    end,
	{noreply, State};

handle_info({http, {RequestId, {{_HTTPVersion, 201, _Text}, _Headers, Body}}}, State) ->
	[{RequestId, Operation, From}] = ets:lookup(tweetphoto, RequestId),
    ets:delete(tweetphoto, RequestId),
    case Operation of
        upload ->
            {struct, Plist} = mochijson2:decode(Body),
            PhotoUrl = proplists:get_value(<<"MediaUrl">>, Plist),
            PhotoId = proplists:get_value(<<"PhotoId">>, Plist),
            gen_server:reply(From, {ok, PhotoUrl, PhotoId});
        _ ->
            pass
    end,
	{noreply, State};

handle_info({http, {RequestId, {{_HTTPVersion, Code, Text}, _Headers, _Body}}}, State) ->
	[{RequestId, Operation, From}] = ets:lookup(tweetphoto, RequestId),
    ets:delete(tweetphoto, RequestId),
    io:format("unexpected response ~p for ~p: ~p ~p~n", [RequestId, Operation, Code, Text]),
    case Operation of
        viewed ->
            pass;
        _ when Operation == metadata ->
            ets:insert(tweetphoto, {From, Operation, [{id, undefined}, {upvotes, undefined}, {downvotes, undefined}, {views, undefined}]}),
            check_for_get_completed(From);
        _ when Operation == voted; Operation == favored; Operation == thumbnail ->
            ets:insert(tweetphoto, {From, Operation, undefined}),
            check_for_get_completed(From);
        _ ->
            gen_server:reply(From, undefined)
    end,
	{noreply, State};

handle_info({http, {RequestId, {error, Reason}}}, State) ->
	[{RequestId, Operation, From}] = ets:lookup(tweetphoto, RequestId),
    ets:delete(tweetphoto, RequestId),
    io:format("error response ~p for ~p reason ~p~n", [RequestId, Operation, Reason]),
    case Operation of
        viewed ->
            pass;
        _ when Operation == metadata ->
            ets:insert(tweetphoto, {From, Operation, [{id, undefined}, {upvotes, undefined}, {downvotes, undefined}, {views, undefined}]}),
            check_for_get_completed(From);
        _ when Operation == voted; Operation == favored; Operation == thumbnail ->
            ets:insert(tweetphoto, {From, Operation, undefined}),
            check_for_get_completed(From);
        _ ->
            gen_server:reply(From, error)
    end,
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
request(voted, PhotoId, Profile) ->
    UserId = proplists:get_value(userid, Profile),
    Url = "http://tweetphotoapi.com/api/tpapi.svc/json/users/" ++ moco_util:to_list(UserId) ++ "/votes/" ++  moco_util:to_list(PhotoId),
	{ok, RequestId} = http:request(get, {Url, []}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId;

request(favored, PhotoId, Profile) ->
    UserId = proplists:get_value(userid, Profile),
    Url = "http://tweetphotoapi.com/api/tpapi.svc/json/users/" ++ moco_util:to_list(UserId) ++ "/favorites/" ++  moco_util:to_list(PhotoId),
	{ok, RequestId} = http:request(get, {Url, []}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId;

request(metadata, PhotoId, _Profile) ->
    Url = "http://tweetphotoapi.com/api/tpapi.svc/json/metadatafromurl?details=false&url=http://tweetphoto.com/" ++  moco_util:to_list(PhotoId),
	{ok, RequestId} = http:request(get, {Url, []}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId;

request(thumbnail, PhotoId, _Profile) ->
    Url = "http://TweetPhotoAPI.com/api/TPAPI.svc/imagefromurl?size=thumbnail&url=http://tweetphoto.com/" ++  moco_util:to_list(PhotoId),
	{ok, RequestId} = http:request(get, {Url, []}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId;

request(viewed, PhotoId, Profile) ->
    Headers = headers(Profile),
    UserId = proplists:get_value(userid, Profile),
    Url = "http://tweetphotoapi.com/api/tpapi.svc/json/users/" ++ moco_util:to_list(UserId) ++ "/views/" ++  moco_util:to_list(PhotoId),
	{ok, RequestId} = http:request(post, {Url, Headers, "application/x-www-form-urlencoded", " "}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_signin(Token, Secret) ->
    Headers = headers(Token, Secret),
	{ok, RequestId} = http:request(get, {"http://tweetphotoapi.com/api/TPAPI.svc/json/oauthsignin", Headers}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_upload(Photo, Message, Profile) ->
    Headers = headers(Profile) ++
              [{"TPUTF8", "True"},
               {"TPMIMETYPE", "image/jpg"},
               {"TPPOST", "False"}] ++
              [{"TPMSG", binary_to_list(base64:encode(Message))} || size(Message) > 0],
	{ok, RequestId} = http:request(post, {"http://tweetphotoapi.com/api/tpapi.svc/json/upload2",
                                          Headers, "application/x-www-form-urlencoded", Photo}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_upvote(PhotoId, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(put, {"http://tweetphotoapi.com/api/tpapi.svc/photos/" ++
                                         moco_util:to_list(PhotoId) ++
                                         "/thumbsup" , Headers, "application/x-www-form-urlencoded", " "},
                                         [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_downvote(PhotoId, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(put, {"http://tweetphotoapi.com/api/tpapi.svc/photos/" ++
                                         moco_util:to_list(PhotoId) ++
                                         "/thumbsdown" , Headers, "application/x-www-form-urlencoded", " "},
                                         [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_favorite(PhotoId, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(post, {"http://tweetphotoapi.com/api/tpapi.svc/users/" ++
                                         moco_util:to_list(proplists:get_value(userid, Profile)) ++ "/favorites/" ++ moco_util:to_list(PhotoId),
                                         Headers, "application/x-www-form-urlencoded", " "}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_unfavorite(PhotoId, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(delete, {"http://tweetphotoapi.com/api/tpapi.svc/users/" ++
                                         moco_util:to_list(proplists:get_value(userid, Profile)) ++ "/favorites/" ++ moco_util:to_list(PhotoId),
                                         Headers}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_delete(PhotoId, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(delete, {"http://tweetphotoapi.com/api/tpapi.svc/photos/" ++
                                            moco_util:to_list(PhotoId), Headers}, [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

tweetphoto_comment(PhotoId, Comment, Profile) ->
    Headers = headers(Profile),
	{ok, RequestId} = http:request(post, {"http://tweetphotoapi.com/api/tpapi.svc/users/" ++
                                         moco_util:to_list(proplists:get_value(userid, Profile)) ++ "/comments/" ++ moco_util:to_list(PhotoId),
                                         Headers, "application/x-www-form-urlencoded", moco_util:to_list(Comment)},
                                         [{timeout, ?HTTP_TIMEOUT}], [{sync, false}]),
    RequestId.

headers(Profile) ->
    headers(proplists:get_value(token, Profile), proplists:get_value(secret, Profile)).

headers(Token, Secret) ->
    Authorization = "Basic " ++ moco_util:to_list(base64:encode(moco_util:to_list(Token) ++ ":" ++ moco_util:to_list(Secret))),
    [{"TPAPIKEY", "398b1a1f-b3e5-46b6-8ac7-6e283dc8fe8c"},
     {"TPISOAUTH", "True"},
     {"TPSERVICE", "Twitter"},
     {"Authorization", Authorization}].

check_for_get_completed(From) ->
    case ets:lookup(tweetphoto, From) of
        Collected when length(Collected) == 4 ->
            ets:delete(tweetphoto, From),
            CompletedResult = element(3, lists:keyfind(metadata, 2, Collected)) ++
                [{O, R} || {_From, O, R} <- Collected, lists:member(O, [voted, favored, thumbnail])],
            gen_server:reply(From, CompletedResult);
        _ ->
            pass
    end.
