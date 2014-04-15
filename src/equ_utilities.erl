-module(equ_utilities).

-export([close_socket/1]).

close_socket(undefined) ->
  ok;
close_socket(Socket) ->
  gen_tcp:close(Socket).