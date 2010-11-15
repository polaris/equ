-module(equ).
-export([start/2, init/4, stop/0]).

-define(SERVER, equ).

start(Num, Port) ->
  Pid = spawn(equ, init, [Num, Port, 'localhost', 80]),
  erlang:register(?SERVER, Pid),
  ok.
  
stop() ->
  ?SERVER ! {stop, self()},
  receive {reply, Reply} -> Reply end.

init(Num, InPort, OutHost, OutPort) ->
  message_loop(initialize(Num, InPort, OutHost, OutPort)).

initialize(Num, InPort, OutHost, OutPort) ->
  case  gen_tcp:listen(InPort, [binary, {packet, 0}, {active, once}]) of
    {ok, Listen} ->
      start_acceptors(Num, Listen, OutHost, OutPort),
      ok;
    {error, _} ->
      io:format("listen failed: posix error~n"),
      error
  end.

message_loop(Data) ->
  receive
    {request, From, Msg} ->
      % {Reply, NewState} = message_handler(Msg, State),
      % reply(From, Reply),
      message_loop(Data);
    {stop, From} ->
      %reply(From, terminate(State))
      From ! {reply, stopped}
  end.
  
start_acceptors(0, _, _, _) ->
  ok;
start_acceptors(Num, Listen, OutHost, OutPort) ->
  spawn(fun() -> acceptor(Listen, OutHost, OutPort) end),
  start_acceptors(Num-1, Listen, OutHost, OutPort).

acceptor(Listen, OutHost, OutPort) ->
  % get configuration from controlling process
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      spawn(fun() -> acceptor(Listen, OutHost, OutPort) end),
      case gen_tcp:connect(OutHost, OutPort, [binary, {packet, 0}, {active, once}]) of
        {ok, Server} ->
          proxy_loop(Client, Server);
        {error, _} ->
          io:format("connect failed: posix error~n"),
          error
      end;
    {error, closed} ->
      io:format("accept failed: socket closed~n"),
      error;
    {error, timeout} ->
      io:format("accept failed: timeout~n"),
      error;
    {error, _} ->
      io:format("accept failed: posix error~n"),
      error
  end.

proxy_loop(Client, Server) ->
  receive
    {tcp, Client, Data} ->
      gen_tcp:send(Server, Data),
      inet:setopts(Client, [{active, once}]),
      proxy_loop(Client, Server);
    {tcp, Server, Data} ->
      gen_tcp:send(Client, Data),
      inet:setopts(Server, [{active, once}]),
      proxy_loop(Client, Server);
    {tcp_closed, _} ->
      ok;
    _ ->
      error
  end.