module Patch :
sig
  type 'flags t = private {
    offset  : int;
    old_len : int;
    new_len : int;
    text    : string;
    flags   : 'flags list;
  }

  val utf8_length : string -> int
  val utf8_offset : string -> ?offset:int -> int -> int
  val make : offset:int -> ?replace:int -> 'flags list -> string -> 'flags t
end

module Socket :
sig
  type 'a controller
  val make : receive:('a -> unit) -> 'a controller

  val set_receive :  'a controller -> ('a -> unit) -> unit
  val set_on_closed : 'a controller -> (unit -> unit) -> unit
  val set_on_connected : 'a controller -> (unit -> unit) -> unit

  val send : 'msg controller -> 'msg -> unit
  val close : 'msg controller -> unit
  val status : 'msg controller -> [ `Pending | `Connected | `Closed ]

  (* Fails with Invalid_argument if one of the pipe is pending or closed *)
  type 'a t
  val endpoint : 'a controller -> 'a t
  val connect : a:'msg t -> b:'msg t -> unit
end

type 'flags patch = 'flags Patch.t

type 'msg socket = 'msg Socket.t
