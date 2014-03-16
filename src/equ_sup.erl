-module(equ_sup).

-behaviour(supervisor).

-export([start_link/2, 
         start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 2307).
-define(NUM_ACCEPTORS, 16).

start_link(Port, NumAcceptors) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, NumAcceptors]).

start_link() ->
  start_link(?DEFAULT_PORT, ?NUM_ACCEPTORS).

init([Port, NumAcceptors]) ->
  BackendServer = {backend_server, {backend_server, start, []}, permanent, 2000, worker, [backend_server]},
  EquServer = {equ_server, {equ_server, start, [Port, NumAcceptors]}, permanent, 2000, worker, [equ_server]},
  Children = [BackendServer, EquServer],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.