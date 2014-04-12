-module(equ_listener).

-behaviour(gen_server).

-export([start_link/0,
         listen/1,
         hangup/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(listener_state, {listen_socket=undefined}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

listen(Port) ->
  gen_server:call(?MODULE, {listen, Port}).

hangup() ->
  gen_server:call(?MODULE, hangup).

init([]) ->
  {ok, #listener_state{}}.

handle_call({listen, Port}, _From, #listener_state{listen_socket = OldListenSocket} = State) ->
  close_socket(OldListenSocket),
  Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Options) of
    {ok, ListenSocket} ->
      {reply, {ok, ListenSocket}, State#listener_state{listen_socket=ListenSocket}};
    {error, Reason} ->
      {reply, {error, Reason}, State#listener_state{listen_socket=undefined}}
  end;
handle_call(hangup, _From, #listener_state{listen_socket = ListenSocket} = State) ->
  close_socket(ListenSocket),
  {reply, ok, State#listener_state{listen_socket=undefined}};
handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(_E, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("equ_listener:handle_info: ~p~n", [_Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, #listener_state{listen_socket = ListenSocket}) ->
  close_socket(ListenSocket),
  ok;
terminate(normal, _State) ->
  ok.

close_socket(undefined) ->
  ok;
close_socket(ListenSocket) ->
  gen_tcp:close(ListenSocket),
  ok.
