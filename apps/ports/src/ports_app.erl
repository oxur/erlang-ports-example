%%%-------------------------------------------------------------------
%% @doc ports public API
%% @end
%%%-------------------------------------------------------------------

-module(ports_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([info/0, ports/0, servers/0, supervisor/0]).

-define(SUPERVISOR, ports_sup).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    logger:set_application_level(goodbyewil, all),
    logger:info("Starting application ...", []),
    ports_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) -> ports_sup:stop(), ok.

supervisor() -> erlang:whereis(?SUPERVISOR).

servers() -> {go_echo, go_echo_server:pid()}.

ports() -> {go, go_echo_server:port()}.

info() ->
    {app, erlang:process_info(self()), supervisor,
     erlang:process_info(supervisor()), go_echo,
     {server, erlang:process_info(go_echo_server:pid()),
      port, erlang:port_info(go_echo_server:port())}}.

%%====================================================================
%% Internal functions
%%====================================================================

