%%%-------------------------------------------------------------------
%% @doc ports top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ports_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    logger:set_application_level(ports, all),
    logger:info("Starting supervisor ...", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    go_server:stop(),
    lisp_server:stop(),
    exit(pid(), shutdown).

init([]) ->
    {ok,
     {#{strategy => one_for_one, intensity => 3,
	period => 60},
      [child(go_echo_server)]}}.

pid() -> erlang:whereis(?MODULE).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

child(Mod) ->
    #{id => Mod, start => {Mod, start_link, []},
      restart => permanent, shutdown => 2000, type => worker,
      modules => [Mod]}.

%%====================================================================
%% Internal functions

