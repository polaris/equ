-module(equ_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         add_backend/2,
         remove_backend/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

add_backend(Address, Port) ->
  gen_event:notify(?SERVER, {add_backend, Address, Port}).

remove_backend(Address, Port) ->
  gen_event:notify(?SERVER, {remove_backend, Address, Port}).
