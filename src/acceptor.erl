-module(acceptor).

-include("records.hrl").

-behaviour(gen_server).

-export([start/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

start(Name, ListenSocket) ->
  gen_server:start_link({local, Name}, ?MODULE, [ListenSocket], []).

init([ListenSocket]) ->
  gen_server:cast(self(), accept),
  {ok, ListenSocket}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(accept, ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Client} ->
      Pid = spawn(fun() -> proxy:start(Client) end),
      gen_tcp:controlling_process(Client, Pid),
      gen_server:cast(self(), accept),
      {noreply, ListenSocket};
    {error, closed} ->
      {stop, normal, ListenSocket};
    {error, timeout} ->
      io:format("accept failed: timeout~n"),
      {stop, error, ListenSocket};
    {error, _} ->
      io:format("accept failed: posix error~n"),
      {stop, error, ListenSocket}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
