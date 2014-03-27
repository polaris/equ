-module(equ_sup).

-behaviour(supervisor).

-export([start_link/2, 
         start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_PORT, 2307).

-define(NUM_ACCEPTORS, 4).

start_link(Port, NumAcceptors) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, NumAcceptors]).

start_link() ->
  start_link(?DEFAULT_PORT, ?NUM_ACCEPTORS).

init([Port, NumAcceptors]) ->
  AcceptorSupervisor = {acceptor_sup, {acceptor_sup, start_link, []}, permanent, 2000, supervisor, [acceptor_sup]},
  ProxySupervisor = {proxy_sup, {proxy_sup, start_link, []}, permanent, 2000, supervisor, [proxy_sup]},
  EventManager = {equ_event, {equ_event, start_link, []}, permanent, 2000, worker, [equ_event]},
  BackendServer = {backend_server, {backend_server, start_link, []}, permanent, 2000, worker, [backend_server]},
  EquServer = {equ_server, {equ_server, start_link, [Port, NumAcceptors]}, permanent, 2000, worker, [equ_server]},
  Children = [EventManager, AcceptorSupervisor, ProxySupervisor, BackendServer, EquServer],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
