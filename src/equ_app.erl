-module(equ_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  case equ_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.
