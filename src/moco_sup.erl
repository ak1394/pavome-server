%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Jun 2008
%%% -------------------------------------------------------------------
-module(moco_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    Log = {moco_log,
    		   {moco_log, start_link, []},
			   permanent, 5000, worker, [moco_log]},

    Database = {moco_db,
    		   {moco_db, start_link, []},
			   permanent, 5000, worker, [moco_db]},

    DatabaseKV = {moco_kv,
    		   {moco_kv, start_link, []},
			   permanent, 5000, worker, [moco_kv]},

    UserSup = {moco_user_sup,
    		        {supervisor, start_link, [{local, moco_user_sup}, moco_user_sup, []]},
			        permanent, 5000, worker, [moco_user_sup]},

    MobileLoginSup = {moco_mobile_login_sup,
    		        {supervisor, start_link, [{local, moco_mobile_login_sup}, moco_mobile_login_sup, []]},
			        permanent, 5000, worker, [moco_mobile_login_sup]},
	
    MobileClientSup = {moco_mobile_session_sup,
    		        {supervisor, start_link, [{local, moco_mobile_session_sup}, moco_mobile_session_sup, []]},
			        permanent, 5000, worker, [moco_mobile_session_sup]},

    Avatar = {moco_avatar,
    		        {moco_avatar, start_link, []},
			        permanent, 5000, worker, [moco_avatar]},

    Twerl = {twerl,
    		        {twerl, start_link, []},
			        permanent, 5000, worker, [twerl]},

    TweetPhoto = {moco_tweetphoto,
    		        {moco_tweetphoto, start_link, []},
			        permanent, 5000, worker, [moco_tweetphoto]},

%    AistTranslit = {moco_aist,
%    		        {moco_aist, start_link, []},
%			        permanent, 5000, worker, [moco_aist]},
	
    Listener = {moco_mobile,
    		        {moco_mobile, start_link, [2222]},
			        permanent, 5000, worker, [moco_mobile]},
    Moco = {moco,
    		   {moco, start_link, []},
			   permanent, 5000, worker, [moco]},

	Processes = [Log, Database, DatabaseKV, UserSup, MobileLoginSup, MobileClientSup, Twerl, Avatar, TweetPhoto, Listener, Moco],

    % Max 1 restart per minute
    {ok, {{one_for_all, 1, 60}, Processes}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
