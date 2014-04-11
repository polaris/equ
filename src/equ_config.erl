-module(equ_config).

-export([new/1,
         get_value/3]).

new(ConfigFile) ->
  ParamsList = read_config(ConfigFile),
  parse_config(ParamsList).

get_value(Key, Dict, Default) ->
  case dict:find(Key, Dict) of
    {ok, [Value]} ->
      Value;
    _ ->
      Default
  end.

read_config(ConfigFile) ->
  case file:consult(ConfigFile) of
    {ok, Config} ->
      hd(Config);
    _ ->
      []
  end.

parse_config(ParamsList) ->
  parse_config(ParamsList, dict:new()).

parse_config([], Dict) ->
  Dict;
parse_config([H|T], Dict) ->
  parse_config(T, dict:append(element(1, H), element(2, H), Dict)).
