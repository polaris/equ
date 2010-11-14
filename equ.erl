-module(equ).
-export([start/0, start/4]).
-export([connect/3]).

start() ->
  start(16, 1234, 'localhost', 80).

start(Num, InPort, OutHost, OutPort) ->
  case  gen_tcp:listen(InPort, [binary, {packet, 0}, {active, once}]) of
    {ok, Listen} ->
      start_servers(Num, Listen, OutHost, OutPort);
    {error, Reason} ->
      {error, Reason}
  end.
  
start_servers(0, _, _, _) ->
  ok;
start_servers(Num, Listen, OutHost, OutPort) ->
  spawn(equ, connect, [Listen, OutHost, OutPort]),
  start_servers(Num-1, Listen, OutHost, OutPort).

connect(Listen, OutHost, OutPort) ->
  {ok, Client} = gen_tcp:accept(Listen),
  spawn(fun() -> connect(Listen, OutHost, OutPort) end),
  {ok, Server} = gen_tcp:connect(OutHost, OutPort, [binary, {packet, 0}, {active, once}]),
  loop(Client, Server).

loop(Client, Server) ->
  receive
    {tcp, Client, Data} ->
      gen_tcp:send(Server, Data),
      inet:setopts(Client, [{active, once}]),
      loop(Client, Server);
    {tcp, Server, Data} ->
      gen_tcp:send(Client, Data),
      inet:setopts(Server, [{active, once}]),
      loop(Client, Server);
    {tcp_closed, _} ->
      ok;
    _ ->
      error
  end.