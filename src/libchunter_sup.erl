
-module(libchunter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [?CHILD(libchunter_server, worker),
           ?CHILD(libchunter_console_sup, supervisor),
           ?CHILD(libchunter_dtrace_sup, supervisor),
           ?CHILD(libchunter_fsm_sup, supervisor)]}}.

