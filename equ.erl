-module(equ).
-export([start/0]).

start() ->
  init(16, 1234, 'localhost', 80).

init(Num, InPort, OutHost, OutPort) ->
  case  gen_tcp:listen(InPort, [binary, {packet, 0}, {active, once}]) of
    {ok, Listen} ->
      start_acceptors(Num, Listen, OutHost, OutPort);
    {error, Reason} ->
      {error, Reason}
  end.
  
start_acceptors(0, _, _, _) ->
  ok;
start_acceptors(Num, Listen, OutHost, OutPort) ->
  spawn(fun() -> acceptor(Listen, OutHost, OutPort) end),
  start_acceptors(Num-1, Listen, OutHost, OutPort).

acceptor(Listen, OutHost, OutPort) ->
  {ok, Client} = gen_tcp:accept(Listen),
  spawn(fun() -> acceptor(Listen, OutHost, OutPort) end),
  {ok, Server} = gen_tcp:connect(OutHost, OutPort, [binary, {packet, 0}, {active, once}]),
  proxy_loop(Client, Server).

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
      {error, unknown_message}
  end.