%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Jun 2008
%%% -------------------------------------------------------------------
-module(moco).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0, start_link/0, start_session/1, stop_user/1, property/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @spec start() -> ok
%% @doc Start the moco server.
start() ->
    ensure_started(crypto),
    ensure_started(ssl),
    ensure_started(inets),
%%    ensure_started(clickatell),
    application:start(moco, permanent).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_user(UserId) ->
    gen_server:cast(?MODULE, {stop_user, UserId}).

start_session(UserId) ->
    gen_server:call(?MODULE, {start_session, UserId}).

property(Section, Key) ->
    {ok, Proplist} = application:get_env(moco, Section),
    proplists:get_value(Key, Proplist).

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
    ets:new(users, [named_table, public]),
    moco_topic:init(),
    moco_web:start_link(),
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
handle_call({start_session, UserId}, _From, State) ->
    P = case ets:lookup(users, UserId) of
        [] ->                                              
            {ok, UserPid} = supervisor:start_child(moco_user_sup, [UserId]),
            ets:insert(users, {UserId, UserPid}),
            UserPid;
        [{_, UserPid}] ->
			case is_process_alive(UserPid) of
				true ->
					UserPid;
				false ->
		    		ets:delete(users, UserId),
            		{ok, NewUserPid} = supervisor:start_child(moco_user_sup, [UserId]),
            		ets:insert(users, {UserId, NewUserPid}),
            		NewUserPid
			end
    end,                                        
    {reply, P, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({stop_user, UserId}, State) ->
    case ets:lookup(users, UserId) of
        [{UserId, UserPid}] ->
            case moco_user:sessions(UserPid) of
                [] ->
                    ets:delete(users, UserId),
                    moco_user:stop(UserPid);
                _ ->
                    io:format("don't stop, some sessions left ~p~n", [UserId])
            end;
        _ ->
            io:format("no user found ~p~n", [UserId])
    end,
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
	%% log4erl:log(warn, "mm_hub, unexpected message: ~p", [Info]),
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

ensure_started(App) ->
    ensure_started(App, []).

ensure_started(App, Env) ->
    application:load(App),
    lists:foreach(fun({Key, Value}) -> application:set_env(App, Key, Value) end, Env),
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

