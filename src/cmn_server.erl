%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(cmn_server).

-behaviour(gen_server).


-include("log.api").
%% API

-export([

	 %% git related
	 git_update_repo/1,
	 git_clone/1, 
	 git_delete/1,
	 git_is_repo_updated/1,
	 
	 %% vm related service
	 vm_get_node/1,
	 vm_check_started/1,
	 vm_check_stopped/1]).


-export([
	 ping/0,
	 start_link/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% git_update_repo
%% @end
%%--------------------------------------------------------------------
-spec git_update_repo(RepoDir::string()) ->  {ok,Info::term()} | {error,Reason::term()}.
git_update_repo(RepoDir) ->
    gen_server:call(?SERVER,{git_update_repo,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% git_update_repo
%% @end
%%--------------------------------------------------------------------
-spec git_clone(GitUrl::string()) -> ok | {error,Reason::term()}.
git_clone(GitUrl) ->
    gen_server:call(?SERVER,{git_clone,GitUrl},infinity).

%%--------------------------------------------------------------------
%% @doc
%% git_update_repo
%% @end
%%--------------------------------------------------------------------
-spec git_delete(RepoDir::string()) -> ok | {error,Reason::term()} .
git_delete(RepoDir) ->
    gen_server:call(?SERVER,{git_delete_repo,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% git_update_repo
%% @end
%%--------------------------------------------------------------------
-spec git_is_repo_updated(RepoDir::string()) -> true | false | {error,Reason::term()} .
git_is_repo_updated(RepoDir) ->
    gen_server:call(?SERVER,{git_is_repo_updated,RepoDir},infinity).



%%--------------------------------------------------------------------
%% @doc
%% vm_get_node
%% @end
%%--------------------------------------------------------------------
-spec vm_get_node(NodeName::string()) -> pong.
vm_get_node(NodeName) ->
    gen_server:call(?SERVER,{vm_get_node,NodeName},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  vm_check_started
%% @end
%%--------------------------------------------------------------------
-spec vm_check_started(Node::atom()) -> true | false.
vm_check_started(Node) ->
    gen_server:call(?SERVER,{vm_check_started,Node},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  vm_check_stopped
%% @end
%%--------------------------------------------------------------------
-spec vm_check_stopped(Node::atom()) -> true | false.
vm_check_stopped(Node) ->
    gen_server:call(?SERVER,{vm_check_stopped,Node},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Ping
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong.

ping() ->
    gen_server:call(?SERVER,{ping},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%%------------------- git
handle_call({git_update_repo,RepoDir}, _From, State) ->
    Reply=lib_git:update_repo(RepoDir),
    {reply, Reply, State};

handle_call({git_clone,GitUrl}, _From, State) ->
    Reply=lib_git:clone(GitUrl),
    {reply, Reply, State};

handle_call({git_delete_repo,RepoDir}, _From, State) ->
    Reply=lib_git:delete(RepoDir),
    {reply, Reply, State};

handle_call({git_is_repo_updated,RepoDir}, _From, State) ->
    Reply=lib_git:is_repo_updated(RepoDir),
    {reply, Reply, State};

%%------------------ vm
handle_call({vm_get_node,NodeName}, _From, State) ->
    Reply=lib_vm:get_node(NodeName),
    {reply, Reply, State};

handle_call({vm_check_started,Node}, _From, State) ->
    Reply=lib_vm:check_started(Node),
    {reply, Reply, State};

handle_call({vm_check_stopped,Node}, _From, State) ->
    Reply=lib_vm:check_stopped(Node),
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    Reply = {error,["Unmatched signal ",Request]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
%%--------------------------------------------------------------------
%% @doc
%% Admin 
%% @end
%%--------------------------------------------------------------------
handle_info({Pid,{ping}}, State) ->
  Pid!{self(),pong},
  {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
