-module(proxy_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ClientSocket, Backend) ->
  supervisor:start_child(?SERVER, [ClientSocket, Backend]).

init([]) ->
  Element = {proxy_server, {proxy_server, start_link, []}, permanent, brutal_kill, worker, [proxy_server]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
