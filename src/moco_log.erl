-module(moco_log).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% Specify levels.
-define(LOG_EMERGENCY, 0). % system is unusable
-define(LOG_ALERT,     1). % action must be taken immediately
-define(LOG_CRITICAL,  2). % critical conditions
-define(LOG_ERROR,     3). % error conditions
-define(LOG_WARNING,   4). % warning conditions
-define(LOG_NOTICE,    5). % normal but significant condition
-define(LOG_INFO,      6). % informational
-define(LOG_DEBUG,     7). % debug-level messages

% facility codes
-define(FAC_KERN,        (0 bsl 3)). % kernel messages
-define(FAC_USER,        (1 bsl 3)). % random user-level messages
-define(FAC_MAIL,        (2 bsl 3)). % mail system
-define(FAC_DAEMON,      (3 bsl 3)). % system daemons
-define(FAC_AUTH,        (4 bsl 3)). % security/authorization messages
-define(FAC_SYSLOG,      (5 bsl 3)). % messages generated internally by syslogd
-define(FAC_LPR,         (6 bsl 3)). % line printer subsystem
-define(FAC_NEWS,        (7 bsl 3)). % network news subsystem
-define(FAC_UUCP,        (8 bsl 3)). % UUCP subsystem
-define(FAC_CRON,        (9 bsl 3)). % clock daemon
-define(FAC_AUTHPRIV,   (10 bsl 3)). % security/authorization messages (private)
-define(FAC_FTP,        (11 bsl 3)). % ftp daemon

% these codes (from 12 through 15) are reserved for system use
%-define(FAC_NTP,   (12 bsl 3)).
%-define(FAC_LOG_ALERT, (13 bsl 3)).
%-define(FAC_LOG_AUDIT, (14 bsl 3)).
%-define(FAC_CLOCK, (15 bsl 3)).

-define(FAC_LOCAL0,     (16 bsl 3)). % reserved for local use
-define(FAC_LOCAL1,     (17 bsl 3)). % reserved for local use
-define(FAC_LOCAL2,     (18 bsl 3)). % reserved for local use
-define(FAC_LOCAL3,     (19 bsl 3)). % reserved for local use
-define(FAC_LOCAL4,     (20 bsl 3)). % reserved for local use
-define(FAC_LOCAL5,     (21 bsl 3)). % reserved for local use
-define(FAC_LOCAL6,     (22 bsl 3)). % reserved for local use
-define(FAC_LOCAL7,     (23 bsl 3)). % reserved for local use


%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, info/1, info/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port, facility}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

info(Message) -> 
    info(default, Message).

info(Channel, Message) ->
    gen_server:cast(?MODULE, {syslog, Channel, ?LOG_INFO, Message}).

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
    {ok, Port} = gen_udp:open(0, [{ip, {127,0,0,1}}]),
    {ok, #state{port=Port, facility=?FAC_USER}}.

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
handle_cast({syslog, Channel, Level, Message}, #state{port=Port, facility=Facility} = State) ->
    Pri = list_to_binary(integer_to_list(Facility bor Level)),
    Header = list_to_binary(atom_to_list(Channel)),
    BinaryMessage = message_to_binary(Message),
    gen_udp:send(Port, {127,0,0,1}, 514, <<"<", Pri/binary, "> ", Header/binary, ": ", BinaryMessage/binary, "\n">>),
    {noreply, State};

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
message_to_binary([H|_] = Message) when is_integer(H) ->
    list_to_binary(Message);

message_to_binary(Message) when is_list(Message) ->
    list_to_binary(string:join([moco_util:to_list(E) || E <- Message], " "));

message_to_binary(_) ->
    <<"bad message">>.
