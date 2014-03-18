-module(acceptor).

-include("records.hrl").

-behaviour(gen_server).

-export([start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

start(ListenSocket) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ListenSocket], []).

init([ListenSocket]) ->
  gen_server:cast(self(), accept),
  {ok, ListenSocket}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, ListenSocket) ->
  {ok, Client} = gen_tcp:accept(ListenSocket),
  Pid = spawn(fun() -> init_proxy(Client) end),
  gen_tcp:controlling_process(Client, Pid),
  gen_server:cast(self(), accept),
  {noreply, ListenSocket}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.

% listen(Listen) ->
%   case gen_tcp:accept(Listen) of
%     {ok, Client} ->
%       Pid = spawn(fun() -> init_proxy(Client) end),
%       gen_tcp:controlling_process(Client, Pid),
%       listen(Listen);
%     {error, closed} ->
%       ok;
%     {error, timeout} ->
%       io:format("accept failed: timeout~n"),
%       error;
%     {error, _} ->
%       io:format("accept failed: posix error~n"),
%       error
%   end.

init_proxy(Client) ->
  #backend{address=OutHost, port=OutPort} = backend_server:get(),
  Options = [binary, {packet, raw}, {active, once}, {nodelay, true}],
  case gen_tcp:connect(OutHost, OutPort, Options) of
    {ok, Server} ->
      proxy_loop(Client, Server);
    {error, _} ->
      io:format("connect failed: posix error~n"),
      error
  end.

proxy_loop(Client, Server) ->
  receive
    {tcp, Client, Data} ->
      gen_tcp:send(Server, Data),
      inet:setopts(Client, [{active, once}]),
      proxy_loop(Client, Server);
    {tcp, Server, Data} ->
      gen_tcp:send(Client, Data),
      inet:setopts(Server, [{active, once}]),
      proxy_loop(Client, Server);
    {tcp_closed, _} ->
      ok;
    _ ->
      error
  end.