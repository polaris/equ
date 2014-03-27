-module(equ_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         add_backend/1,
         remove_backend/1]).

-define(SERVER, ?MODULE).

-include("records.hrl").

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

add_backend(Backend) ->
  gen_event:notify(?SERVER, {add_backend, Backend}).

remove_backend(Backend) ->
  gen_event:notify(?SERVER, {remove_backend, Backend}).
