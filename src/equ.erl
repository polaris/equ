-module(equ).

-export([start/0, stop/0]).

start() ->
  case application:start(?MODULE) of
    ok -> 
      init_config();
    {error, Reason} -> 
      io:format("Failed to start equ: ~p~n", [Reason])
  end.

stop() ->
  application:stop(?MODULE).

init_config() ->
  event_logger:add_handler(),
  configure_backend().

configure_backend() ->
  case application:get_env(?MODULE, backend_servers) of
    {ok, List} ->
      add_backend_server(List);
    _ ->
      ok
  end.

add_backend_server([]) ->
  ok;
add_backend_server([H|T]) ->
  backend_server:add(element(1, H), element(2, H)),
  add_backend_server(T).