-module(equ).

-export([start/0,
         start/1,
         stop/0]).

-include("global.hrl").

start() ->
  start(?DEFAULT_CONFIG).

start(ConfigFile) ->
  case application:start(?MODULE) of
    ok -> 
      event_logger:add_handler(),
      
      Config = equ_config:new(ConfigFile),
      
      configure_backend(Config),
      
      Port = equ_config:get_value(port, Config, ?DEFAULT_PORT),
      NumAcceptors = equ_config:get_value(num_acceptors, Config, ?DEFAULT_NUM_ACCEPTORS),
      Timeout = equ_config:get_value(proxy_timeout, Config, ?DEFAULT_PROXY_TIMEOUT),
      
      start(Port, NumAcceptors, Timeout);
    {error, Reason} -> 
      io:format("Failed to start equ: ~p~n", [Reason])
  end.

stop() ->
  application:stop(?MODULE).

configure_backend(Config) ->
  List = equ_config:get_value(backend_servers, Config, []),
  add_backend_server(List).

add_backend_server([]) ->
  ok;
add_backend_server([H|T]) ->
  backend_list:add(element(1, H), element(2, H)),
  add_backend_server(T).

start(Port, NumAcceptors, Timeout) ->
  SpawnFun = fun(ClientSocket) ->
    proxy_server:start_link(ClientSocket, Timeout)
  end,
  case equ_listener:listen(Port) of
    {ok, ListenSocket} ->
      start_acceptors(NumAcceptors, ListenSocket, SpawnFun);
    {error, Reason} ->
      io:format("Failed to listen on port ~p: ~p~n", [Port, Reason])
  end.

start_acceptors(NumAcceptors, _ListenSocket, _SpawnFun) when NumAcceptors =< 0 ->
  ok;
start_acceptors(NumAcceptors, ListenSocket, SpawnFun) ->
  acceptor_server:start(ListenSocket, SpawnFun),
  start_acceptors(NumAcceptors-1, ListenSocket, SpawnFun).
