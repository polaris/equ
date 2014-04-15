-module(backend_list).

-behaviour(gen_server).

-export([start_link/0,
         stop/0,
         add/2,
         remove/2,
         get/0,
         count/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(backend_state, {backend_list=[], backend_map=dict:new()}).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

add(Address, Port) ->
  gen_server:cast(?MODULE, {add, Address, Port}).

remove(Address, Port) ->
  gen_server:cast(?MODULE, {remove, Address, Port}).

get() ->
  gen_server:call(?MODULE, get).

count() ->
  gen_server:call(?MODULE, count).

init([]) ->
  {ok, #backend_state{}}.

handle_call(get, _From, #backend_state{backend_list=[]} = State) ->
  {reply, {error, empty}, State};
handle_call(get, _From, #backend_state{backend_list=List} = State) ->
  Backend = hd(List),
  NewList = right_rotate(List),
  {reply, {ok, Backend}, State#backend_state{backend_list=NewList}};
handle_call(count, _From, #backend_state{backend_list=List} = State) ->
  Count = length(List),
  {reply, {ok, Count}, State}.

right_rotate([H|T]) ->
  lists:append(T, [H]);
right_rotate([]) ->
  [].

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({add, Address, Port}, #backend_state{backend_list=List, backend_map=Dict} = State) ->
  {ok, Backend} = backend_server:start_link(Address, Port),
  NewList = [Backend|List], 
  NewDict = dict:append({Address, Port}, Backend, Dict),
  {noreply, State#backend_state{backend_list=NewList, backend_map=NewDict}};
handle_cast({remove, Address, Port}, #backend_state{backend_list=List, backend_map=Dict} = State) ->
  {ok, [Backend]} = dict:find({Address, Port}, Dict),
  NewList = lists:delete(Backend, List),
  NewDict = dict:erase({Address, Port}, Dict),
  {noreply, State#backend_state{backend_list=NewList, backend_map=NewDict}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-ifdef(TEST).

simple_add_test() ->
  backend_list:start_link(),
  backend_list:add('1.2.3.4', 1234),
  ?assert(backend_list:count() == {ok, 1}),
  {ok, Backend} = backend_list:get(),
  ?assertEqual(backend_server:get_address(Backend), {ok, '1.2.3.4'}),
  ?assertEqual(backend_server:get_port(Backend), {ok, 1234}),
  backend_list:stop().

count_test() ->
  backend_list:start_link(),
  ?assertEqual(backend_list:count(), {ok, 0}),
  backend_list:add('1.2.3.4', 1234),
  ?assertEqual(backend_list:count(), {ok, 1}),
  backend_list:add('1.2.3.5', 1234),
  ?assertEqual(backend_list:count(), {ok, 2}),
  backend_list:remove('1.2.3.4', 1234),
  ?assertEqual({ok, 1}, backend_list:count()),
  backend_list:remove('1.2.3.5', 1234),
  ?assertEqual({ok, 0}, backend_list:count()),
  backend_list:stop().

-endif.