-module(equ_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  BackendServer = {backend_server, {backend_server, start, []}, permanent, 2000, worker, [backend_server]},
  EquServer = {equ_server, {equ_server, start, [1234, 16]}, permanent, 2000, worker, [equ_server]},
  Children = [BackendServer, EquServer],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.