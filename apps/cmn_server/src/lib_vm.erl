%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_vm).
  

 
%% API

-export([
	 get_node/1,
	 check_started/1,
	 check_stopped/1

	]).

-define(NumTries,500).
-define(SleepInterval,20).


%%%===================================================================
%%% API
%%%===================================================================

get_node(NodeName)->
    {ok,Host}=net:gethostname(),
    list_to_atom(NodeName++"@"++Host).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

check_started(Node)->
    check_started(Node,?NumTries,?SleepInterval,false).    

 check_started(_Node,_NumTries,_SleepInterval,true)->
    true;
 check_started(_Node,0,_SleepInterval,Result)->
    Result;
 check_started(Node,NumTries,SleepInterval,false)->
    case net_adm:ping(Node) of
	pang->
	    timer:sleep(SleepInterval),
	    NewN=NumTries-1,
	    Result=false;
	pong->
	    NewN=NumTries-1,
	    Result=true
    end,
    check_started(Node,NewN,SleepInterval,Result). 

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_stopped(Node)->
    check_stopped(Node,?NumTries,?SleepInterval,false).    

 check_stopped(_Node,_NumTries,_SleepInterval,true)->
    true;
 check_stopped(_Node,0,_SleepInterval,Result)->
    Result;
 check_stopped(Node,NumTries,SleepInterval,false)->
    case net_adm:ping(Node) of
	pong->
	    timer:sleep(SleepInterval),
	    NewN=NumTries-1,
	    Result=false;
	pang->
	    NewN=NumTries-1,
	    Result=true
    end,
    check_stopped(Node,NewN,SleepInterval,Result).    
	    


%%%===================================================================
%%% Internal functions
%%%===================================================================
