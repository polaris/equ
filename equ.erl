-module(proxy).
-export([start/3]).

start(InPort, OutHost, OutPort) ->
  {ok, Listen} = gen_tcp:listen(InPort, [binary, {packet, 0}, {active, once}]),
  spawn(fun() -> connect(Listen, OutHost, OutPort) end).

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
      ok
  end.