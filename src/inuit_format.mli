module Make (M : sig type flag end) :
sig
  type cursor = M.flag Inuit.cursor

  val formatter_of_cursor : cursor -> Format.formatter

  val push_cursor : (cursor -> cursor) -> Format.formatter -> unit
  val push_flags : (M.flag list -> M.flag list) -> Format.formatter -> unit
  val pop : Format.formatter -> unit

  val with_cursor :
    (cursor -> cursor) ->
    (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a -> 'b

  val with_flags :
    (M.flag list -> M.flag list) ->
    (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a -> 'b
end
