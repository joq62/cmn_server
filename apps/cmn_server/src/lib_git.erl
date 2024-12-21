%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_git).
  
-define(UpToDate,"Up to date").
-define(NotUpToDate,"Not up to date").
 
%% API

-export([
	 update_repo/1,
	 clone/1, 
	 delete/1,
	 is_repo_updated/1

	]).



%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_repo_updated(RepoDir)->
    case filelib:is_dir(RepoDir) of
	false->
	    {error,["RepoDir doesnt exists, need to clone"]};
	true->
	    case is_up_to_date(RepoDir) of
		{error,Reason}->
		    {error,Reason};
		IsUpdated ->
		    IsUpdated
	    end
    end.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_repo(RepoDir)->
    case filelib:is_dir(RepoDir) of
	false->
	    {error,["Dir eexists ",RepoDir]};
	true->
	    case fetch_merge(RepoDir) of
		{error,Reason}->
		    {error,Reason};
		{ok,Info}->
		    {ok,Info}
	    end
    end.
	     
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone(RepoGit)->
    case os:cmd("git clone -q "++RepoGit) of
	[]->
	    ok;
	Error->
	    {error,["Failed to clone ",RepoGit,Error]}
    end.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
delete(RepoDir)->
    file:del_dir_r(RepoDir).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
fetch_merge(LocalRepo)->
    Result=case is_up_to_date(LocalRepo) of
	       false->
		   []=os:cmd("git -C "++LocalRepo++" "++"fetch origin "),
		   Info=os:cmd("git -C "++LocalRepo++" "++"merge  "),
		   {ok,Info};
	       true->
		   {error,["Already updated ",LocalRepo]}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_up_to_date(LocalRepo)->
    _Fetch=os:cmd("git -C "++LocalRepo++" "++"fetch origin "),
    Status=os:cmd("git -C "++LocalRepo++" status -uno | grep -q 'Your branch is up to date'  && echo Up to date || echo Not up to date"),
    [FilteredGitStatus]=[S||S<-string:split(Status, "\n", all),
			  []=/=S],
    Result=case FilteredGitStatus of
	       ?UpToDate->
		   true;
	       ?NotUpToDate->
		   false;
	       _ -> %[\"fatal: not a git repository (or any of the parent directories): .git\",\"Not up to date\"]
		   {error,["Unmatched signal ",FilteredGitStatus]}
	   end,
    Result.
