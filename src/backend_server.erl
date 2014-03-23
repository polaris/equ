-module(backend_server).

-behaviour(gen_server).

-export([start_link/0,
         stop/0,
         add/2,
         remove/2,
         get/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  configure_backend(),
  {ok, []}.
  
configure_backend() ->
  case application:get_env(backend_servers) of
    {ok, List} -> add_backend_server(List);
    _ -> ok
  end.

add_backend_server([]) -> ok;
add_backend_server([H|T]) ->
  add(element(1, H), element(2, H)),
  add_backend_server(T).

add(Address, Port) ->
  gen_server:cast(?MODULE, {add, Address, Port}).

remove(Address, Port) ->
  gen_server:cast(?MODULE, {remove, Address, Port}).

get() ->
  gen_server:call(?MODULE, get).

right_rotate([H|T]) ->
  lists:append(T, [H]);
right_rotate([]) ->
  [].

handle_call(get, _From, []) ->
  {reply, {error, empty}, []};
handle_call(get, _From, [H|T]) ->
  NewState = right_rotate([H|T]),
  {reply, {ok, H}, NewState}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add, Address, Port}, State) ->
  NewState = [#backend{address=Address, port=Port}|State],  
  {noreply, NewState};
handle_cast({remove, Address, Port}, State) ->
  NewState = lists:delete(#backend{address=Address, port=Port}, State),
  {noreply, NewState}.
    
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
