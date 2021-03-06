%%%-------------------------------------------------------------------
%% @doc otplog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(otplog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Server = {log_server, {log_server, start_link, []},
	permanent, 2000, worker, [log_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
