open Inuit

type 'a clickable = [> `Clickable | `Clicked ] as 'a

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
  type 'flags t

  val make  : 'flags cursor -> 'flags t
  val add   : ?children:('flags t -> unit) ->
              ?action:('flags cursor -> unit) ->
              ?opened:bool ref -> 'flags clickable t -> 'flags cursor
  val clear : 'flags t -> unit
end
