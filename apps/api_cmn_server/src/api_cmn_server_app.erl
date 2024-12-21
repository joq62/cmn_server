%%%-------------------------------------------------------------------
%% @doc api_cmn_server public API
%% @end
%%%-------------------------------------------------------------------

-module(api_cmn_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    api_cmn_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
