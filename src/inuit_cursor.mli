open Inuit_base

(** A [cursor] is very like an [Inuit_region.t] with a default set of flags.
    This is a convenient type to expose to higher-level code.

    Since it is just a wrapper, you can refer to [Inuit_region.t] for
    the documentation of most functions.
*)
type 'flags cursor

(** The set of flags for buffers with the capability to mark areas as clickable
    and to receive clicks. *)
type 'flags clickable = [> `Clickable | `Clicked] as 'flags

(** A dummy cursor that is always closed. Useful as a placeholder. *)
val null : _ cursor

(** [text cursor ?flags str] append the text [str] at the right of [cursor]
    while moving it to right.
    If [flags] is not provided, default set of flags is applied. *)
val text    : 'flags cursor -> ?flags:'flags list -> string -> unit

(** [clear cursor] erase the content of the region containing the cursor.
    New content can still be added, using [text] for instance.  *)
val clear   : 'flags cursor -> unit

(** Erase the content and close the cursor.
    No content can be added anymore. *)
val kill    : 'flags cursor -> unit

(** [sub cursor] creates an empty sub-cursor at the right end of [cursor].
    Clearing the sub-cursor will not affect other content, but clearing
    [cursor] will kill the sub-cursor. *)
val sub     : 'flags cursor -> 'flags cursor

(** Create a sub-cursor and associate an observer call-back.
    See [Inuit_region.observer] and [Inuit_region.observe] for more
    information. *)
val observe : 'flags cursor ->
  ('flags cursor -> [`Local | `Remote] -> 'flags patch ->
   'flags list * (unit -> unit) option) -> 'flags cursor

(** A cursor is closed if it has no observers, all changes will be ignored.
    This can happen when using [kill], or [clear] on a parent cursor, and when
    the root socket is closed. *)
val is_closed  : 'flags cursor -> bool

(** The region affected by this cursor *)
val region     : 'flags cursor -> 'flags Inuit_region.t

(** {1 Manipulating flags} *)

(** A cursor is nothing but a region with a default set of flags.
    The following functions allow to manipulate this set. *)

(** Add a flag, [mem_flag flag (add_flag flag c) = true]. *)
val add_flag : 'flags -> 'flags cursor -> 'flags cursor

(** Remove a flag, [mem_flag flag (rem_flag flag c) = false]. *)
val rem_flag : 'flags -> 'flags cursor -> 'flags cursor

(** Check if a flag is in the set of flags of the cursor. *)
val mem_flag : 'flags -> 'flags cursor -> bool

(** Direct access to the list of flags. *)
val get_flags : 'flags cursor -> 'flags list

(** Replace the set of flags with a new one. *)
val with_flags : 'flags list -> 'flags cursor -> 'flags cursor

(** {1 Manipulating indentation} *)

(** Get indentation level of cursor *)
val get_indent : 'flags cursor -> int

(** Set indentation level of cursor *)
val with_indent : 'flags cursor -> int -> 'flags cursor

(** Add the argument to indentation level, clamp negative values to 0.
    [shift_ident t n = with_indent t (max 0 (get_indent t + n))] *)
val shift_indent : 'flags cursor -> int -> 'flags cursor

(** {1 Creating content (convenience functions)} *)

(** Create a sub-cursor that can be clicked.
    The call-back provided as argument is invoked during a click. *)
val clickable : 'flags clickable cursor ->
  ('flags cursor -> unit) -> 'flags cursor

(** Append formatted text to the cursor *)
val printf : 'flags cursor -> ?flags:'flags list ->
  ('a, unit, string, unit) format4 -> 'a

(** A mix of [printf] and [clickable]: append formatted text that can be
    clicked.
    The callback is provided last, for instance:
    [link cursor "Click to visit %s" "https://github.com" (fun _ -> ...)] *)
val link : 'flags clickable cursor -> ?flags:'flags list ->
  ('a, unit, string, ('flags cursor -> unit) -> unit) format4 -> 'a

(** Make a cursor from a region. *)
val cursor_of_region : ?flags:'flags list -> ?indent:int ->
  'flags  Inuit_region.t -> 'flags cursor

(** [Inuit_region.make] wrapper: return a root cursor and a pending socket.
    Socket must be connected prior to changing the cursor. *)
val make : unit -> 'flags cursor * 'flags patch socket
