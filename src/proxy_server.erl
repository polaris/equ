-module(proxy_server).

-include("records.hrl").

-behaviour(gen_server).

-export([start_link/2, 
         start/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link(ClientSocket, Backend) ->
  gen_server:start_link(?MODULE, [ClientSocket, Backend], []).

start(ClientSocket, Backend) ->
  Result = proxy_sup:start_child(ClientSocket, Backend),
  gen_tcp:controlling_process(ClientSocket, element(2, Result)),
  Result.

init([ClientSocket, Backend]) ->
  gen_server:cast(self(), connect),
  {ok, {ClientSocket, Backend}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(connect, {ClientSocket, Backend}) ->
  #backend{address=OutHost, port=OutPort} = Backend,
  Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
  case gen_tcp:connect(OutHost, OutPort, Options) of
    {ok, ServerSocket} ->
      {noreply, {ClientSocket, ServerSocket}};
    {error, _} ->
      io:format("connect failed: posix error~n"),
      {stop, error, {ClientSocket, nosocket}}
  end;

handle_info({tcp, ClientSocket, Data}, {ClientSocket, ServerSocket}) ->
  gen_tcp:send(ServerSocket, Data),
  inet:setopts(ClientSocket, [{active, once}]),
  {noreply, {ClientSocket, ServerSocket}};
handle_info({tcp, ServerSocket, Data}, {ClientSocket, ServerSocket}) ->
  gen_tcp:send(ClientSocket, Data),
  inet:setopts(ServerSocket, [{active, once}]),
  {noreply, {ClientSocket, ServerSocket}};
handle_info(_Info, State) ->
  io:format("handle_info: ~p~n", [_Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
