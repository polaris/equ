-module(acceptor_server).

-behaviour(gen_server).

-export([start_link/2,
         start/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(acceptor_state, {listen_socket, spawn_fun}).

start_link(ListenSocket, SpawnFun) ->
  gen_server:start_link(?MODULE, [ListenSocket, SpawnFun], []).

start(ListenSocket, SpawnFun) ->
  acceptor_sup:start_child(ListenSocket, SpawnFun).

init([ListenSocket, SpawnFun]) ->
  gen_server:cast(self(), accept),
  {ok, #acceptor_state{listen_socket = ListenSocket, spawn_fun = SpawnFun}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(accept, #acceptor_state{listen_socket = ListenSocket, spawn_fun = SpawnFun} = State) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, ClientSocket} ->
      handle_accept(ClientSocket, SpawnFun),
      gen_server:cast(self(), accept),
      {noreply, State};
    {error, closed} ->
      {stop, normal, State};
    {error, timeout} ->
      {stop, error, State};
    {error, _} ->
      {stop, error, State}
  end.

handle_accept(ClientSocket, SpawnFun) ->
  {ok, Pid} = SpawnFun(ClientSocket),
  gen_tcp:controlling_process(ClientSocket, Pid).

handle_info(_Info, State) ->
  io:format("acceptor_server:handle_info: ~p~n", [_Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
