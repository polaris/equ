-module(equ_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  EventManager = {equ_event, {equ_event, start_link, []}, permanent, 2000, worker, [equ_event]},
  AcceptorSupervisor = {acceptor_sup, {acceptor_sup, start_link, []}, permanent, 2000, supervisor, [acceptor_sup]},
  Listener = {equ_listener, {equ_listener, start_link, []}, permanent, 2000, worker, [equ_listener]},
  BackendServer = {backend_server, {backend_server, start_link, []}, permanent, 2000, worker, [backend_server]},
  Children = [EventManager, AcceptorSupervisor, Listener, BackendServer],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
