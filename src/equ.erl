-module(equ).

-export([start/0]).

start() ->
  application:start(?MODULE).
