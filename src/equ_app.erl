-module(equ_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  case equ_sup:start_link() of
    {ok, Pid} ->
      event_logger:add_handler(),
      backend_server:read_configure(),
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.
