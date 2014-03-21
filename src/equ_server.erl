-module(equ_server).

-include("records.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start/2, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(InPort, NumAcceptors) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [InPort, NumAcceptors], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([InPort, NumAcceptors]) ->
  acceptor_sup:start_link(),
  configure_backend(),
  Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
  case gen_tcp:listen(InPort, Options) of
    {ok, Listen} ->
      start_acceptors(NumAcceptors, Listen),
      {ok, Listen};
    {error, Reason} ->
      {stop, Reason}
  end.

configure_backend() ->
  case application:get_env(backend_servers) of
    {ok, List} -> add_backend_server(List);
    _ -> ok
  end.

add_backend_server([]) -> ok;
add_backend_server([H|T]) ->
  backend_server:add(element(1, H), element(2, H)),
  add_backend_server(T).

start_acceptors(NumAcceptors, _Listen) when NumAcceptors =< 0 ->
  ok;
start_acceptors(NumAcceptors, Listen) ->
  acceptor_sup:start_child(Listen),
  start_acceptors(NumAcceptors-1, Listen).


handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  gen_tcp:close(State),
  io:format("socket closed~n"),
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.