(* Minimal text API *)

class type cursor =
  object ('self)
    method text : string -> int -> int -> unit
    method clear : unit

    method sub : 'self
    method sub_action : ('self -> unit) option -> 'self

    method is_closed : bool
  end

type 'a action = 'a -> unit
  constraint 'a = #cursor

val text   : #cursor -> string -> unit
val clear  : #cursor -> unit
val sub    : ?action:'cursor action option -> 'cursor -> 'cursor

val link   : 'cursor -> string -> 'cursor action -> unit

val printf : #cursor -> ('a, unit, string, unit) format4 -> 'a

val null_cursor : cursor
val is_closed   : #cursor -> bool

(* Basic widgets *)

module Nav : sig
  type 'cursor t
    constraint 'cursor = #cursor

  val make  : 'cursor -> string -> ('cursor t -> unit) -> unit
  val modal : 'cursor t -> string -> ('cursor t -> unit) -> unit

  val title : 'cursor t -> 'cursor
  val body  : 'cursor t -> 'cursor

  val null : cursor t
end

module Tree : sig
  type 'cursor t
    constraint 'cursor = #cursor
  val make  : 'cursor -> 'cursor t
  val add   : ?children:('cursor t -> unit) -> ?action:'cursor action option ->
              ?opened:bool ref -> 'cursor t -> 'cursor
  val clear : 'cursor t -> unit

  val null  : cursor t
end
