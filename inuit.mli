include (module type of struct include Inuit_base end)

type 'flags cursor

val text    : 'flags cursor -> ?flags:'flags list -> string -> unit
val clear   : 'flags cursor -> unit
val sub     : 'flags cursor -> 'flags cursor
val observe : 'flags cursor ->
  ('flags cursor -> side -> 'flags patch -> (unit -> unit) option) -> 'flags cursor

val is_closed    : 'flags cursor -> bool
val default      : 'flags cursor -> 'flags list
val with_default : 'flags list -> 'flags cursor -> 'flags cursor
val region       : 'flags cursor -> 'flags region

val action : ([> `Clickable | `Clicked] as 'flags) cursor ->
  ('flags cursor -> unit) -> 'flags cursor

val printf : 'flags cursor -> ?flags:'flags list ->
  ('a, unit, string, unit) format4 -> 'a

val link : ([> `Clickable | `Clicked] as 'flags) cursor ->
  ?flags:'flags list -> ('flags cursor -> unit) ->
  ('a, unit, string, unit) format4 -> 'a
