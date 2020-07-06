-module(go_echo_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

-export([pid/0, port/0, port_info/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([send/1]).

-define(SERVER, ?MODULE).

-define(DELIMITER, [10]).

-define(GO_TIMEOUT, 100).

-define(GO_BIN,
	"go/src/github.com/geomyidia/erlang-port-examp"
	"les/bin/echo").

%%====================================================================
%% API functions
%%====================================================================

stop() -> gen_server:call(?MODULE, stop).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
			  []).

pid() -> erlang:whereis(?MODULE).

port() -> gen_server:call(?MODULE, port).

port_info() -> erlang:port_info(port()).

send(Msg) -> gen_server:call(?MODULE, Msg).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_Args) ->
    process_flag(trap_exit, true), {ok, create_port()}.

handle_call(stop, _From, Port) ->
    stop_port(Port), {stop, normal, ok, Port};
handle_call(port, _From, Port) -> {reply, Port, Port};
handle_call(Msg, _From, Port) ->
    Msg_bin = term_to_binary(Msg),
    logger:debug("Sending echo data: ~p~n", [Msg_bin]),
    Port ! {self(), {command, [Msg_bin, ?DELIMITER]}},
    Data = util:receive_line(Port, ?GO_TIMEOUT),
    {reply, binary_to_term(Data, [safe]), Port}.

handle_cast(_Msg, Port) -> {noreply, Port}.

handle_info({'EXIT', _From, normal}, Port) ->
    logger:error("The Go echo server is exiting (normal)."),
    {noreply, Port};
handle_info({'EXIT', _From, shutdown}, Port) ->
    logger:error("The Go echo server is exiting (shutdown)."),
    {noreply, Port};
handle_info({'EXIT', From, Reason}, Port) ->
    io:format("Go echo process: ~p exited with reason: "
	      "~p~n",
	      [From, Reason]),
    {noreply, Port};
handle_info(Info, Port) ->
    logger:error("The Go echo server is handling info "
		 "of type: ~p.",
		 [Info]),
    {noreply, Port}.

terminate(normal, _Port) ->
    logger:error("The Go echo server is terminating.");
terminate(shutdown, _Port) ->
    logger:error("The supervisor is shutting down the "
		 "Go echo server.");
terminate(Reason, _Port) ->
    logger:error("The Go echo server is terminating for "
		 "reason: ~p.",
		 [Reason]).

code_change(_OldVsn, Port, _Extra) -> {ok, Port}.

%%====================================================================
%% Internal functions
%%====================================================================

create_port() ->
    util:create_port(filename:join(util:priv_dir(),
				   ?GO_BIN),
		     []).

stop_port(Port) -> Port ! {self(), close}.
