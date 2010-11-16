-module(equ).
-export([start/0, init/2, add_backend/2, stop/0]).
-export([message_loop/1]).

-define(SERVER, equ).

start() ->
  Pid = spawn(equ, message_loop, [[]]),
  erlang:register(?SERVER, Pid),
  ok.
  
stop() ->
  ?SERVER ! {stop, self()},
  receive {reply, Reply} -> Reply end.

add_backend(Host, Port) ->
  ?SERVER ! {add_backend, self(), {Host, Port}},
  receive {reply, Reply} -> Reply end.

init(Num, InPort) ->
  ?SERVER ! {initialize, self(), {Num, InPort}},
  receive {reply, Reply} -> Reply end.
%  initialize(Num, InPort),
%  message_loop([]).

initialize(Num, InPort) ->
  case gen_tcp:listen(InPort, [binary, {packet, 0}, {active, once}]) of
    {ok, Listen} ->
      start_acceptors(Num, Listen),
      ok;
    {error, _} ->
      io:format("listen failed: posix error~n"),
      error
  end.

left(List, Times) ->
  left(List, Times, []).

left([], Times, Acc) when Times > 0 ->
  left(lists:reverse(Acc), Times, []);
left(List, 0, Acc) ->
  List ++ lists:reverse(Acc);
left([H|T], Times, Acc) ->
  left(T, Times-1, [H|Acc]).

message_loop(Data) ->
  receive
    %{request, From, Msg} ->
      %{Reply, NewState} = message_handler(Msg, State),
      %reply(From, Reply),
      %message_loop(Data);
    {initialize, From, Msg} ->
      {Num, InPort} = Msg,
      initialize(Num, InPort),
      From ! {reply, ok},
      message_loop(Data);
    {get_backend, From} ->
      [Backend|_] = Data,
      From ! {reply, Backend},
      NewData = left(Data, 1),
      message_loop(NewData);
    {add_backend, From, Backend} ->
      NewData = [Backend|Data],
      From ! {reply, ok},
      message_loop(NewData);
    {stop, From} ->
      %reply(From, terminate(State))
      From ! {reply, stopped}
  end.
  
start_acceptors(0, _) ->
  ok;
start_acceptors(Num, Listen) ->
  spawn(fun() -> acceptor(Listen) end),
  start_acceptors(Num-1, Listen).

backend() ->
  ?SERVER ! {get_backend, self()},
  receive {reply, Backend} -> Backend end.

acceptor(Listen) ->
  {OutHost, OutPort} = backend(),
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      spawn(fun() -> acceptor(Listen) end),
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