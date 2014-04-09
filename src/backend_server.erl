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

-record(backend_state, {backend_list=[]}).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

init([]) ->
  {ok, #backend_state{}}.

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

handle_call(get, _From, #backend_state{backend_list=[]} = State) ->
  {reply, {error, empty}, State};
handle_call(get, _From, #backend_state{backend_list=List} = State) ->
  Backend = hd(List),
  NewList = right_rotate(List),
  {reply, {ok, Backend}, State#backend_state{backend_list=NewList}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add, Address, Port}, #backend_state{backend_list=List} = State) ->
  Backend = #backend{address=Address, port=Port},
  NewList = [Backend|List], 
  equ_event:add_backend(Backend), 
  {noreply, State#backend_state{backend_list=NewList}};
handle_cast({remove, Address, Port}, #backend_state{backend_list=List} = State) ->
  Backend = #backend{address=Address, port=Port},
  NewList = lists:delete(Backend, List),
  equ_event:remove_backend(Backend),
  {noreply, State#backend_state{backend_list=NewList}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
