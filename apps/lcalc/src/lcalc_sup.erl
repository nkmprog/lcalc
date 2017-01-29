%%%-------------------------------------------------------------------
%% @doc lcalc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lcalc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Parser = ?CHILD(lcalc_worker, worker, []),
    {ok, { {one_for_one, 5, 10}, [Parser]} }.

%%====================================================================
%% Internal functions
%%====================================================================
