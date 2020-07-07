-module(lisp_echo_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

-export([pid/0, port/0, port_info/0]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-export([send/1]).

-define(SERVER, ?MODULE).

-define(DELIMITER, [10]).

-define(LISP_COMMAND, "sbcl").

-define(LISP_SERVER,
	"cl-port-examples/echo/src/main.lisp").

-define(LISP_TIMEOUT, 100).

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
    process_flag(trap_exit, true),
    {ok, create_port()}.

handle_call(stop, _From, Port) ->
    stop_port(Port), 
    {stop, normal, ok, Port};
handle_call(port, _From, Port) -> {reply, Port, Port};
handle_call(Msg, From, Port) ->
    Msg_bin = term_to_binary(Msg),
    logger:debug("Sending data: ~p~n", [Msg_bin]),
    Port ! {self(), {command, [Msg_bin, ?DELIMITER]}},
    Data = util:receive_line(Port, ?LISP_TIMEOUT),
    logger:debug("Got data: ~p~n", [Data]),
    case Data of
        <<>> ->  
            logger:error("Got empty data from ~p; continuing ....~n", [From]),
            {reply, nodata, Port};
        _ -> 
            logger:debug("Data: ~p", [Data]),
            {reply, binary_to_term(Data, [safe]), Port}
    end.

handle_cast(_Msg, Port) -> {noreply, Port}.

handle_info({'EXIT', _From, normal}, Port) ->
    logger:error("The Lisp echo server is exiting (normal)."),
    {noreply, Port};
handle_info({'EXIT', _From, shutdown}, Port) ->
    logger:error("The Lisp echo server is exiting (shutdown)."),
    {noreply, Port};
handle_info({'EXIT', From, Reason}, Port) ->
    io:format("Lisp echo process: ~p exited with reason: ~p~n",
	      [From, Reason]),
    {noreply, Port};
handle_info({Port, {exit_status, Status}}, Port) ->
    logger:error("Port exited with status ~p; restarting",
                                  [Status]),
    {noreply, create_port()};
handle_info(Info, Port) -> 
    logger:error("The Lisp echo server is handling info of type: ~p.", [Info]),
    {noreply, Port}.

terminate(normal, _Port) ->
    logger:error("The Lisp echo server is terminating.");
terminate(shutdown, _Port) ->
    logger:error("The supervisor is shutting down the Lisp echo server.");
terminate(Reason, _Port) ->
    logger:error("The Lisp echo server is terminating for reason: ~p.", [Reason]).

code_change(_OldVsn, Port, _Extra) -> {ok, Port}.

%%====================================================================
%% Internal functions
%%====================================================================

create_port() ->
  util:create_port(?LISP_COMMAND, 
      "--script " ++ filename:join(util:priv_dir(), ?LISP_SERVER)).

stop_port(Port) ->
    Port ! {self(), close}.
