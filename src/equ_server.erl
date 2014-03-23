-module(equ_server).

-behaviour(gen_server).

-export([start_link/2, 
         stop/0]).

-export([init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link(Port, NumAcceptors) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, NumAcceptors], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([Port, NumAcceptors]) ->
  Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Options) of
    {ok, ListenSocket} ->
      start_acceptors(NumAcceptors, ListenSocket),
      {ok, ListenSocket};
    {error, Reason} ->
      {stop, Reason}
  end.

start_acceptors(NumAcceptors, _ListenSocket) when NumAcceptors =< 0 ->
  ok;
start_acceptors(NumAcceptors, ListenSocket) ->
  acceptor_server:start(ListenSocket),
  start_acceptors(NumAcceptors-1, ListenSocket).

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  gen_tcp:close(State),
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
