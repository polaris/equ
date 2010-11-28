-module(equ_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Server = {equ_server, {equ_server, start, [1234, 16]}, permanent, 2000, worker, [equ_server]},
  Children = [Server],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.