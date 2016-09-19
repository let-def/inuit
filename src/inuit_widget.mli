open Inuit_cursor

type 'a clickable = [> `Clickable | `Clicked ] as 'a
type 'a editable = [> `Editable | `Prompt ] as 'a

module Nav : sig
  type 'flags t

  type 'flags frame = {
    title: 'flags cursor;
    body: 'flags cursor;
    nav: 'flags t;
  }

  val make : string -> ('flags frame -> unit) -> 'flags t

  val push : 'flags t -> string -> ('flags frame -> unit) -> unit
  val goto : 'flags t -> string -> ('flags frame -> unit) -> unit

  val render : 'flags clickable t -> 'flags cursor -> unit
end

module Tree : sig
  type 'flags t = 'flags cursor

  val make  : 'flags cursor -> 'flags t
  val add   : ?children:('flags t -> unit) ->
              ?action:('flags cursor -> unit) ->
              ?opened:bool ref -> 'flags clickable t -> 'flags cursor
  val clear : 'flags t -> unit
end

module Check : sig
  type 'flags t

  val make : ?state:bool -> ?on_change:('flags t -> unit) ->
    'flags clickable cursor -> 'flags t

  val change : 'flags t -> state:bool -> unit

  val state : 'flags t -> bool
end

module Edit :
sig
  type 'flags t

  val make : ?state:string -> ?on_change:('flags t -> unit) ->
    'flags editable cursor -> unit

  val change : 'flags t -> state:string -> unit

  val state : 'flags t -> string
end

module Slider :
sig
  type 'flags t

  val make : ?state:int * int -> ?on_change:('flags t -> unit) ->
    'flags editable cursor -> unit

  val change : 'flags t -> state:int * int -> unit

  val state : 'flags t -> int * int
end
