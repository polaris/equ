-module(acceptor_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(NumAcceptors, ListenSocket) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [NumAcceptors, ListenSocket]).

init([NumAcceptors, ListenSocket]) ->
  Children = get_acceptor_configs(NumAcceptors, ListenSocket),
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

get_acceptor_configs(NumAcceptors, Listen) ->
  get_acceptor_configs(NumAcceptors, Listen, []).

get_acceptor_configs(0, _Listen, Acc) ->
  Acc;
get_acceptor_configs(NumAcceptors, Listen, Acc) ->
  Name = list_to_atom("pid" ++ integer_to_list(NumAcceptors)),
  Acceptor = {Name, {acceptor, start, [Name, Listen]}, permanent, 2000, worker, [acceptor]},
  get_acceptor_configs(NumAcceptors-1, Listen, [Acceptor | Acc]).
