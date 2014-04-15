-module(backend_server).

-behaviour(gen_server).

-export([start_link/2,
         stop/1,
         get_address/1,
         get_port/1,
         add_client/1,
         remove_client/1,
         client_count/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(backend_server_state, {address, port, count = 0}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link(Address, Port) ->
  gen_server:start_link(?MODULE, [Address, Port], []).

stop(Pid) ->
  gen_server:cast(Pid, stop).

get_address(Pid) ->
  gen_server:call(Pid, get_address).

get_port(Pid) ->
  gen_server:call(Pid, get_port).

add_client(Pid) ->
  gen_server:cast(Pid, add_client).

remove_client(Pid) ->
  gen_server:cast(Pid, remove_client).

client_count(Pid) ->
  gen_server:call(Pid, client_count).

init([Address, Port]) ->
  {ok, #backend_server_state{address = Address, port = Port}}.

handle_call(get_address, _From, #backend_server_state{address = Address} = State) ->
  {reply, {ok, Address}, State};
handle_call(get_port, _From, #backend_server_state{port = Port} = State) ->
  {reply, {ok, Port}, State};
handle_call(client_count, _From, #backend_server_state{count = Count} = State) ->
  {reply, {ok, Count}, State};
handle_call(_Message, _From, State) ->
  {reply, {ok, foo}, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(add_client, #backend_server_state{count = Count} = State) ->
  {noreply, State#backend_server_state{count = Count + 1}};
handle_cast(remove_client, #backend_server_state{count = Count} = State) ->
  {noreply, State#backend_server_state{count = max(0, Count - 1)}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-ifdef(TEST).

create_test() ->
  ok.

-endif.
