-module(proxy).

-include("records.hrl").

-export([listen/1]).

listen(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      Pid = spawn(fun() -> init_proxy(Client) end),
      gen_tcp:controlling_process(Client, Pid),
      listen(Listen);
    {error, closed} ->
      ok;
    {error, timeout} ->
      io:format("accept failed: timeout~n"),
      error;
    {error, _} ->
      io:format("accept failed: posix error~n"),
      error
  end.

init_proxy(Client) ->
  #backend{address=OutHost, port=OutPort} = backend_server:get(),
  Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
  case gen_tcp:connect(OutHost, OutPort, Options) of
    {ok, Server} ->
      proxy_loop(Client, Server);
    {error, _} ->
      io:format("connect failed: posix error~n"),
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