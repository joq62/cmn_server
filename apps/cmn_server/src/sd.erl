%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(sd).
  

 
%% API


-export([
       
	 cast/2,
	 call/3
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

call(control,Msg,TimeOut)->
    {ok,Hostname}=net:gethostname(),
    ServerId=list_to_atom(atom_to_list(control)++"@"++Hostname),
    call(ServerId,Msg,TimeOut);
call(log,Msg,TimeOut)->
    {ok,Hostname}=net:gethostname(),
    ServerId=list_to_atom(atom_to_list(log)++"@"++Hostname),
    call(ServerId,Msg,TimeOut);


call(ServerId,Msg,TimeOut)->
    case global:whereis_name(ServerId) of
	undefined->
	    {error,[undefined,ServerId]};
	Pid->
	    Self=self(),
	    Pid!{Self,Msg},
	    receive
		{Pid,Reply}->
		    Reply
	    after 
		TimeOut ->
		    {error,["Timeout in call",ServerId,Msg,TimeOut]}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

cast(control,Msg)->
    {ok,Hostname}=net:gethostname(),
    ServerId=list_to_atom(atom_to_list(control)++"@"++Hostname),
    cast(ServerId,Msg);
cast(log,Msg)->
    {ok,Hostname}=net:gethostname(),
    ServerId=list_to_atom(atom_to_list(log)++"@"++Hostname),
    cast(ServerId,Msg);

cast(ServerId,Msg)->
    Self=self(),
    case rpc:cast(node(),global,send,[ServerId,{Self,Msg}]) of
	{badrpc,Reason}->
	    {error,[?MODULE,?LINE,badrpc,Reason]};
	Result->
	    Result
    end.



%%%===================================================================
%%% Internal functions
%%%===================================================================
