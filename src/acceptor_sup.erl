-module(acceptor_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ListenSocket, Fun) ->
  supervisor:start_child(?SERVER, [ListenSocket, Fun]).

init([]) ->
  Element = {acceptor_server, {acceptor_server, start_link, []}, transient, brutal_kill, worker, [acceptor_server]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
