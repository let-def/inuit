(* TODO: Validate UTF-8 strings *)

(* At the lowest level, Inuit buffer are represented by a stream of patches.

   The concrete content is never materialized nor stored, but you can do that
   by folding over the stream of patches.

   Each patch expresses the replacement of a region, represented as an offset
   and a length, by a new piece of text.
   Offsets and length are expressed in number of unicode sequences, not bytes
   nor characters.
*)


module Patch :
sig

  type operation =
    | Remove  of int
    | Insert  of string
    | Replace of int * string
    | Propertize of int

  (** A [flags Patch.t] represents the replacement of a piece of text by
      another, possibly annotated by values of type [flags]. *)
  type 'flags t = private
    { offset    : int         (** Starting at [offset]'th unicode sequence *)
    ; operation : operation
    ; text_len  : int
    ; flags     : 'flags list (** A list of backend defined [flags].       *)
    }

  (** [utf8_length str] is the number of unicode sequences in [str].

      [str] is assumed to be a valid utf-8 string, result is undefined
      otherwise. *)
  val utf8_length : string -> int

  (** [utf8_offset str ?offset index] returns the offset as a number of bytes
      of the [index]th unicode sequences in [str].

      If [offset] is provided, search starts from this byte. Otherwise it
      defaults to 0.
      If [index] is 0, 0 is returned.
      If [index] is one more than the number of sequences in [str],
      [String.length str] is returned.
      If [index] is even more than that, exception [Not_found] is thrown.

      [str] is assumed to be a valid utf-8 string, result is undefined
      otherwise. *)
  val utf8_offset : string -> ?offset:int -> int -> int

  (** Produces a patch, ensuring that [new_len = utf8_length text].
      TODO: validate utf-8 string. *)
  val make : offset:int -> 'flags list -> operation -> 'flags t

  (** Replace the flags in a patch by a new list *)
  val with_flags : 'flags list -> 'flags t -> 'flags t

  val removed : _ t -> int
  val inserted : _ t -> int
  val inserted_text : _ t -> string
end

module Socket :
sig
  (* Sockets are the basic objects used to interconnect producers and consumers
     of a buffer.

     A [Socket.controller] is used internally by producers and consumers to
     notify or to get notified of changes.

     A [Socket.t] is used at a higher-level to connect things together.
  *)

  (** A [message Socket.controller] is a bidirectional stream of [message].
      The creator of a value of this type can:
      - be notified when connection is established or lost,
      - be notified when a value of type [message] is received,
      - send messages to the other end,
      - terminates the connection.
  *)
  type 'a controller

  (** [make ~receive] creates a new controller that will call [receive] when a
      message is received. *)
  val make : receive:('a -> unit) -> 'a controller

  (** [set_receive] sets the callback invoked when a message is received.*)
  val set_receive :  'a controller -> ('a -> unit) -> unit
  (** [set_on_closed] sets the callback invoked when the connection terminates.
      Closing is definitive (it can happen at most once). *)
  val set_on_closed : 'a controller -> (unit -> unit) -> unit
  (** [set_on_connected] sets the callback invoked when the connection is
      established. It can be invoked at most once. *)
  val set_on_connected : 'a controller -> (unit -> unit) -> unit

  (** [send ctrl msg] sends one message to the other end.
      [status (endpoint ctrl)] should be [`Connected] for this to succeed.
      Fail with [Invalid_argument] otherwise.  *)
  val send : 'msg controller -> 'msg -> unit

  (** [close ctrl] terminates the connection now.
      If [status (endpoint ctrl)] is [`Pending] or [`Connected], it is updated
      to [`Closed] and [on_closed] callback is invoked.
      If [status (endpoint ctrl)] is already [`Closed], nothing happens. *)
  val close : 'msg controller -> unit

  (** A handle exposed to higher-level code to connect sockets together. *)
  type 'a t
  val endpoint : 'a controller -> 'a t

  (** Get the status of the socket.
      Possible statuses are:
      - [`Pending], connection has not been established yet, no message
        should be sent yet.
      - [`Connected], connection has been established, messages can be sent.
      - [`Closed], connection is terminated, no messages can be sent anymore.
  *)
  val status : 'msg t -> [ `Pending | `Connected | `Closed ]

  (** Connect two sockets together.  Both should be in [`Pending] status.
      Fail with [Invalid_argument] otherwise.  *)
  val connect : a:'msg t -> b:'msg t -> unit
end

type 'flags patch = 'flags Patch.t

type 'msg socket = 'msg Socket.t
