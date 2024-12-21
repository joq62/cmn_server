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
git_test()->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
   
    


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
