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

start_link(ClientSocket, Timeout) ->
  gen_server:start_link(?MODULE, [ClientSocket, Timeout], []).

init([ClientSocket, Timeout]) ->
  case inet:peername(ClientSocket) of
    {ok, {Address, Port}} ->
      io:format("Client ~p:~p connected.~n", [Address, Port]),
      case backend_list:get() of
        {ok, Backend} ->
          gen_server:cast(self(), connect),
          {ok, #proxy_state{client_socket=ClientSocket, backend=Backend, timeout=Timeout}};
        {error, Reason} ->
          io:format("Failed to get backend: ~p~n", [Reason]),
          gen_tcp:close(ClientSocket),
          {stop, ignore}
      end;
    {error, Reason} ->
      io:format("Failed to resolve remote address and port: ~p~n", [Reason]),
      {stop, ignore}
  end.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(connect, #proxy_state{backend=Backend, timeout=Timeout} = State) ->
  {ok, Address} = backend_server:get_address(Backend),
  {ok, Port} = backend_server:get_port(Backend),
  Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
  case gen_tcp:connect(Address, Port, Options) of
    {ok, ServerSocket} ->
      backend_server:add_client(Backend),
      {noreply, State#proxy_state{server_socket=ServerSocket}, Timeout};
    {error, Reason} ->
      io:format("connect failed: ~p~n", [Reason]),
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
  {stop, normal, State};
handle_info(_Info, State) ->
  io:format("proxy_server:handle_info: ~p~n", [_Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, #proxy_state{backend=Backend, client_socket=ClientSocket, server_socket=ServerSocket}) ->
  backend_server:remove_client(Backend),
  equ_utilities:close_socket(ClientSocket),
  equ_utilities:close_socket(ServerSocket).
