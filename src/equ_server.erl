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
  backend_server:add('www.google.de', 80),
  backend_server:add('www.yahoo.de', 80),
  Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
  case gen_tcp:listen(InPort, Options) of
    {ok, Listen} ->
      start_acceptors(NumAcceptors, Listen),
      {ok, Listen};
    {error, Reason} ->
      {stop, Reason}
  end.

start_acceptors(0, _) ->
  ok;
start_acceptors(NumAcceptors, Listen) ->
  spawn(proxy, listen, [Listen]),
  start_acceptors(NumAcceptors-1, Listen).

handle_call(_Request, _From, State) ->
  Reply = 123,
  {reply, Reply, State}.

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