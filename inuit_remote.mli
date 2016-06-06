open Inuit_base

type revision = { remote : int; local : int; }

type 'flag remote_patch =
  | Ack of revision
  | Patch of revision * 'flag patch

val make : unit -> 'flag remote_patch socket * 'flag patch socket
