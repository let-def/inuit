open Inuit_base

(** FIXME: order between clients is not competely specified (hardcoded in emacs
    case).
    A conflict resolution protocol.
    It lifts a synchronous socket, where patches should always be received and
    sent sequentially, into an asynchronous one where patches are tagged with a
    version number and can be concurrently sent and received.  *)
type revision = { remote : int; local : int; }

type 'flag remote_patch =
  | Ack of revision
  | Patch of revision * 'flag patch

val make : unit -> 'flag remote_patch socket * 'flag patch socket
