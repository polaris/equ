-module(event_logger).

-behaviour(gen_event).

-export([add_handler/0,
         delete_handler/0]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {}).

add_handler() ->
  equ_event:add_handler(?MODULE, []).

delete_handler() ->
  equ_event:delete_handler(?MODULE, []).

init([]) ->
  {ok, #state{}}.

handle_event({add_backend, Address, Port}, State) ->
  error_logger:info_msg("added backend: ~p:~p", [Address, Port]),
  {ok, State};
handle_event({remove_backend, Address, Port}, State) ->
  error_logger:info_msg("remove backend: ~p:~p", [Address, Port]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
