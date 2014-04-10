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

simple_test() ->
  ok = application:start(equ),
  ?assertNot(undefined == whereis(equ_sup)).

-endif.