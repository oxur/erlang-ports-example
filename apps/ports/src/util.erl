-module(util).

-export([create_port/2, priv_dir/0, receive_line/2]).

-define(APPNAME, ports).

create_port(Cmd, Args) ->
    Prog = io_lib:format("~s ~s", [Cmd, Args]),
    open_port({spawn, Prog}, [binary, exit_status, {line, 1}]).

priv_dir() ->
  case code:priv_dir(?APPNAME) of
    {error, _} -> logger:critical("~w priv dir not found~n", [?APPNAME]), exit(error);
    PrivDir -> PrivDir
  end.

receive_line(Port, Timeout) ->
    receive_line(Port, Timeout, []).

receive_line(Port, Timeout, Buffer) ->
    receive
      {Port, {exit_status, ExitStatus}} ->
        logger:error("Port unexpectedly exited with status ~p", [ExitStatus]),
        erlang:term_to_binary({error, port_exit});
      {Port, {data, <<"\n">>}} ->
        logger:debug("Skipping newline ...", []),
        receive_line(Port, Timeout, Buffer);
      {Port, {data, {_Flag, Data}}} ->
        logger:debug("Got data: ~p", [Data]),
        receive_line(Port, Timeout, [Data | Buffer]);
      {Port, Msg} ->
        logger:debug("Got message: ~p", [Msg]),
        receive_line(Port, Timeout, [Msg | Buffer])
          after Timeout -> 
            logger:debug("Buffer: ~p", [Buffer]),
            Rev = lists:reverse(Buffer),
            logger:debug("Reversed: ~p", [Rev]),
            Bin = binary:list_to_bin(Rev),
            logger:debug("Binary: ~p", [Bin]),
            Bin
    end.
