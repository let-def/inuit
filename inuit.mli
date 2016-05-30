include (module type of struct include Inuit_base end)

type 'flags cursor

module Cursor : sig
  type 'flags clickable = [> `Clickable | `Clicked] as 'flags

  val null : _ cursor

  val text    : 'flags cursor -> ?flags:'flags list -> string -> unit
  val clear   : 'flags cursor -> unit
  val kill    : 'flags cursor -> unit
  val sub     : 'flags cursor -> 'flags cursor
  val observe : 'flags cursor ->
    ('flags cursor -> side -> 'flags patch -> (unit -> unit) option) -> 'flags cursor

  val is_closed  : 'flags cursor -> bool

  val region     : 'flags cursor -> 'flags region

  val add_flag   : 'flags -> 'flags cursor -> 'flags cursor
  val rem_flag   : 'flags -> 'flags cursor -> 'flags cursor
  val mem_flag   : 'flags -> 'flags cursor -> bool
  val get_flags  : 'flags cursor -> 'flags list
  val with_flags : 'flags list -> 'flags cursor -> 'flags cursor

  val clickable : 'flags clickable cursor ->
    ('flags cursor -> unit) -> 'flags cursor

  val printf : 'flags cursor -> ?flags:'flags list ->
    ('a, unit, string, unit) format4 -> 'a

  val link : 'flags clickable cursor -> ?flags:'flags list ->
       ('a, unit, string, ('flags cursor -> unit) -> unit) format4 -> 'a

  val cursor_of_region : ?flags:'flags list -> 'flags region -> 'flags cursor

  val make : unit -> 'flags cursor * 'flags pipe
end
