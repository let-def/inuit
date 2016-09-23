open Inuit_cursor

type 'flags trace

val null : 'flags trace

val initial : ?depth:int -> 'flags cursor -> 'flags trace

val call :
  string ->
  ('a -> string) ->
  ('b -> string) -> ('flags clickable trace -> 'a -> 'b) -> 'flags clickable trace -> 'a -> 'b

val fix :
  string ->
  ('a -> string) ->
  ('b -> string) -> (('a -> 'b) -> 'a -> 'b) ->
  'flags clickable trace -> 'a -> 'b
