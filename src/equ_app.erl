-module(equ_app).

-behaviour(application).

-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(_Type, _StartArgs) ->
  equ_sup:start_link().

stop(_State) ->
  ok.


-ifdef(TEST).

start_test() ->
  ok = application:start(equ),
  ?assertNot(undefined == whereis(equ_sup)),
  ok = application:stop(equ).

stop_test() ->
  ok = application:start(equ),
  ok = application:stop(equ),
  ?assert(undefined == whereis(equ_sup)).

-endif.