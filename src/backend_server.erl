-module(backend_server).

-include("records.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start/0, stop/0, add/2, get/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  {ok, []}.
  
add(Address, Port) ->
  gen_server:cast(?MODULE, {add, Address, Port}).

get() ->
  gen_server:call(?MODULE, get).

right_rotate([H|T]) ->
  lists:append(T, [H]);
right_rotate([]) ->
  [].

handle_call(get, _From, []) ->
  {reply, error, []};
handle_call(get, _From, [H|T]) ->
  NewState = right_rotate([H|T]),
  {reply, H, NewState}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add, Address, Port}, State) ->
  NewState = [#backend{address=Address, port=Port}|State],  
  {noreply, NewState}.
    
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.