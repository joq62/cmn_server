%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).

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
   
    io:format("Test OK !!! ~p~n",[?MODULE]),
   % timer:sleep(2000),
   % init:stop(),
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
   
    file:del_dir_r(?RepoDir),
    %% repo doesnt exists
    {error,["RepoDir doesnt exists, need to clone","cmn_server"]}=lib_git:is_repo_updated(?RepoDir),
    {error,["Dir eexists ","cmn_server"]}=lib_git:delete(?RepoDir),
    {error,["Dir eexists ","cmn_server"]}=lib_git:update_repo(?RepoDir),

    {error,["RepoDir doesnt exists, need to clone","not_exists"]}=lib_git:is_repo_updated(?RepoDirNotExists),
    {error,["Dir eexists ","not_exists"]}=lib_git:delete(?RepoDirNotExists),
    {error,["Dir eexists ","not_exists"]}=lib_git:update_repo(?RepoDirNotExists),
    
    %% Clone 
    {error,["Failed to clone ","https://github.com/joq62/url_not_exists.git",
	    "fatal: could not read Username for 'https://github.com': No such device or address\n"]}=lib_git:clone(?GitUrlNotExists),
    ok=lib_git:clone(?GitUrl),
    true=lib_git:is_repo_updated(?RepoDir),
    {error,["Allready updated ","cmn_server"]}=lib_git:update_repo(?RepoDir),
    ok=lib_git:delete(?RepoDir),
    {error,["RepoDir doesnt exists, need to clone","cmn_server"]}=lib_git:is_repo_updated(?RepoDir),
    
    
    
    
    %%


    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
 
 
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
    
    ok.
