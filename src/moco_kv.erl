-module(moco_kv).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,
         put/3,
         put_file/3,
         get/2,
         info/2,
         delete/2
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {ns}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Namespace, Key, Value) ->
    gen_server:call(?MODULE, {put, Namespace, prepare(Key), Value}, infinity).

put_file(Namespace, Key, Filename) ->
    {ok, BytesCopied} = gen_server:call(?MODULE, {put_file, Namespace, prepare(Key), Filename}, infinity),
    BytesCopied.

get(Namespace, Key) ->
    gen_server:call(?MODULE, {get, Namespace, prepare(Key)}, infinity).

info(Namespace, Key) ->
    gen_server:call(?MODULE, {info, Namespace, prepare(Key)}, infinity).

delete(Namespace, Key) ->
    gen_server:call(?MODULE, {delete, Namespace, prepare(Key)}, infinity).

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
    {ok, #state{ns=[
					   {[avatar, twitter], {"priv/data/avatar/twitter/", ".png"}},
					   
					   {[audio, amr], {"priv/data/audio/amr/", ".amr"}},
					   {[audio, mp4], {"priv/data/audio/mp4/", ".mp4"}},
					   {[audio, stlow], {"priv/data/audio/stlow/", ".mp4"}},
					   
					   {[video, thumbnail], {"priv/data/video/thumbnail/", ".jpg"}},
					   {[video, preview], {"priv/data/video/preview/", ".jpg"}},
					   {[video, flv], {"priv/data/video/flv/", ".flv"}},
					   {[video, stlow], {"priv/data/video/stlow/", ".3gp"}},
					   {[video, orig], {"priv/data/video/orig/", ".3gp"}},

					   {[image, thumbnail], {"priv/data/image/thumbnail/", ".jpg"}},
					   {[image, small], {"priv/data/image/small/", ".jpg"}},
					   {[image, big], {"priv/data/image/big/", ".jpg"}},
					   {[image, orig], {"priv/data/image/orig/", ".jpg"}},

					   {[preview, image, preview], {"priv/data/preview/image/preview/", ".jpg"}},
					   {[preview, image, orig], {"priv/data/preview/image/orig/", ".jpg"}}
					   ]}}.

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

handle_call({put, Namespace, Key, Value}, _From, State) ->
    {Path, Ext} = proplists:get_value(Namespace, State#state.ns),
    ok = file:write_file(Path ++ Key ++ Ext, Value),
    {reply, ok, State};

handle_call({put_file, Namespace, Key, Filename}, _From, State) ->
    {Path, Ext} = proplists:get_value(Namespace, State#state.ns),
    NewFilename = Path ++ Key ++ Ext,
    case file:copy(Filename, NewFilename) of
        {ok, BytesCopied} ->
            {reply, {ok, BytesCopied} , State};
        {error, Error} ->
            {reply, {error, Error} , State}
    end;

handle_call({get, Namespace, Key}, _From, State) ->
    {Path, Ext} = proplists:get_value(Namespace, State#state.ns),
    case file:read_file(Path ++ Key ++ Ext) of
    	{ok, Value} ->
    		{reply, Value, State};
        {error,enoent} ->
    		{reply, undefined, State}
    end;

handle_call({info, Namespace, Key}, _From, State) ->
    {Path, Ext} = proplists:get_value(Namespace, State#state.ns),
    case file:read_file_info(Path ++ Key ++ Ext) of
    	{ok, FileInfo} ->
            Size = FileInfo#file_info.size,
            Time = FileInfo#file_info.mtime,
    		{reply, [{size, Size}, {time, Time}], State};
        {error, _Reason} ->
    		{reply, undefined, State}
    end;

handle_call({delete, Namespace, Key}, _From, State) ->
	{Path, Ext} = proplists:get_value(Namespace, State#state.ns),
    ok = file:delete(Path ++ Key ++ Ext),
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

prepare(Key) when is_integer(Key) ->
	integer_to_list(Key);

prepare(Key) when is_atom(Key) ->
	atom_to_list(Key);

prepare(Key) ->
	Key.
