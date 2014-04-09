-module(equ).

-export([start/0, stop/0]).

-define(DEFAULT_CONFIG, 'equ.config').
-define(DEFAULT_PORT, 2307).
-define(NUM_ACCEPTORS, 4).

start() ->
  case application:start(?MODULE) of
    ok -> 
      event_logger:add_handler(),
      init_config(),
      listen(?DEFAULT_PORT, ?NUM_ACCEPTORS);
    {error, Reason} -> 
      io:format("Failed to start equ: ~p~n", [Reason])
  end.

stop() ->
  application:stop(?MODULE).

init_config() ->
  ParamsList = read_config(),
  Config = parse_config(ParamsList),
  configure_backend(Config).

read_config() ->
  case file:consult(?DEFAULT_CONFIG) of
    {ok, Config} ->
      hd(Config);
    _ ->
      []
  end.

parse_config(ParamsList) ->
  parse_config(ParamsList, dict:new()).

parse_config([], Dict) ->
  Dict;
parse_config([H|T], Dict) ->
  parse_config(T, dict:append(element(1, H), element(2, H), Dict)).

configure_backend(Config) ->
  case dict:find(backend_servers, Config) of
    {ok, [List]} ->
      add_backend_server(List);
    _ ->
      ok
  end.

add_backend_server([]) ->
  ok;
add_backend_server([H|T]) ->
  backend_server:add(element(1, H), element(2, H)),
  add_backend_server(T).

listen(Port, NumAcceptors) ->
  Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Options) of
    {ok, ListenSocket} ->
      start_acceptors(NumAcceptors, ListenSocket);
    {error, Reason} ->
      io:format("Failed to listen on port ~p: ~p~n", [Port, Reason])
  end.

start_acceptors(NumAcceptors, _ListenSocket) when NumAcceptors =< 0 ->
  ok;
start_acceptors(NumAcceptors, ListenSocket) ->
  acceptor_server:start(ListenSocket),
  start_acceptors(NumAcceptors-1, ListenSocket).
