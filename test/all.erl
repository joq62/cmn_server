%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).

-include("log.api").

%%---------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=init(),

    ok=git_test(),
%    ok=vm_test(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    ?LOG_NOTICE("Start ",[?MODULE,?FUNCTION_NAME,?LINE]),

    timer:sleep(2000),
%    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(GitUrl,"https://github.com/joq62/cmn_server.git").
-define(GitUrlNotExists,"https://github.com/joq62/url_not_exists.git").

-define(RepoDir,"cmn_server").
-define(RepoDirNotExists,"not_exists").

git_test()->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ?LOG_NOTICE("Start ",[?MODULE,?FUNCTION_NAME,?LINE]),
    timer:sleep(1000),
   
    file:del_dir_r(?RepoDir),
    %% repo doesnt exists
    {error,["RepoDir doesnt exists, need to clone","cmn_server"]}=api_cmn_server:git_is_repo_updated(?RepoDir),
    {error,["Dir eexists ","cmn_server"]}=api_cmn_server:git_delete(?RepoDir),
    {error,["Dir eexists ","cmn_server"]}=api_cmn_server:git_update_repo(?RepoDir),

    {error,["RepoDir doesnt exists, need to clone","not_exists"]}=api_cmn_server:git_is_repo_updated(?RepoDirNotExists),
    {error,["Dir eexists ","not_exists"]}=api_cmn_server:git_delete(?RepoDirNotExists),
    {error,["Dir eexists ","not_exists"]}=api_cmn_server:git_update_repo(?RepoDirNotExists),
    
    %% Clone 
    {error,["Failed to clone ","https://github.com/joq62/url_not_exists.git",
	    "fatal: could not read Username for 'https://github.com': No such device or address\n"]}=api_cmn_server:git_clone(?GitUrlNotExists),
    ok=api_cmn_server:git_clone(?GitUrl),
    true=api_cmn_server:git_is_repo_updated(?RepoDir),
    {error,["Allready updated ","cmn_server"]}=api_cmn_server:git_update_repo(?RepoDir),
    ok=api_cmn_server:git_delete(?RepoDir),
    {error,["RepoDir doesnt exists, need to clone","cmn_server"]}=api_cmn_server:git_is_repo_updated(?RepoDir),
 
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(NodeName,"nodename").

vm_test()->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ?LOG_NOTICE("Start ",[?MODULE,?FUNCTION_NAME,?LINE]),
    timer:sleep(1000),

    %% get_node(NodeName)
    {ok,HostName}=net:gethostname(),
    Node=list_to_atom(?NodeName++"@"++HostName),
    
    Node=api_cmn_server:vm_get_node(?NodeName),
    true=api_cmn_server:vm_check_stopped(Node),
    false=api_cmn_server:vm_check_started(Node),
    
    TestNode=node(),
    false=api_cmn_server:vm_check_stopped(TestNode),
    true=api_cmn_server:vm_check_started(TestNode),

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ?LOG_NOTICE("Start ",[?MODULE,?FUNCTION_NAME,?LINE]),
    timer:sleep(1000),
 
    ok=application:start(cmn_server),
   % sequence by the application 
    CmnServerPid=whereis(cmn_server),
    yes=global:register_name(cmn_server,CmnServerPid),
   
    pong=sd:call(cmn_server,{ping},5000),
    ok=application:start(api_cmn_server), 
    pong=api_cmn_server:ping(),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
  
    os:cmd("rm -r logs *.Mnesia"),
    ok=application:start(log), 
    pong=log:ping(),
    ?LOG_NOTICE("Start ",[?MODULE,?FUNCTION_NAME,?LINE]),
    timer:sleep(1000),
    ok.
