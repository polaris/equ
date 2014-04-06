-module(proxy_server).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(proxy_state, {backend=undefined, client_socket=undefined, server_socket=undefined, timeout=infinity}).

-define(DEFAULT_TIMEOUT, 60000).

-include("records.hrl").

start_link(ClientSocket, Backend) ->
  gen_server:start_link(?MODULE, [ClientSocket, Backend], []).

init([ClientSocket, Backend]) ->
  gen_server:cast(self(), connect),
  {ok, #proxy_state{client_socket=ClientSocket, backend=Backend, timeout=?DEFAULT_TIMEOUT}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(connect, #proxy_state{backend=Backend, timeout=Timeout} = State) ->
  #backend{address=OutHost, port=OutPort} = Backend,
  Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
  case gen_tcp:connect(OutHost, OutPort, Options) of
    {ok, ServerSocket} ->
      {noreply, State#proxy_state{server_socket=ServerSocket}, Timeout};
    {error, _} ->
      io:format("connect failed: posix error~n"),
      {stop, error, State}
  end.

handle_info({tcp, ClientSocket, Data}, #proxy_state{client_socket=ClientSocket, server_socket=ServerSocket, timeout=Timeout} = State) ->
  gen_tcp:send(ServerSocket, Data),
  inet:setopts(ClientSocket, [{active, once}]),
  {noreply, State, Timeout};
handle_info({tcp, ServerSocket, Data}, #proxy_state{client_socket=ClientSocket, server_socket=ServerSocket, timeout=Timeout} = State) ->
  gen_tcp:send(ClientSocket, Data),
  inet:setopts(ServerSocket, [{active, once}]),
  {noreply, State, Timeout};
handle_info(timeout, State) ->
  #proxy_state{client_socket=ClientSocket, server_socket=ServerSocket} = State,
  gen_tcp:close(ClientSocket),
  gen_tcp:close(ServerSocket),
  {stop, normal, State};
handle_info(_Info, State) ->
  io:format("handle_info: ~p~n", [_Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
