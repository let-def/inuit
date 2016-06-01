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

type 'flags patch = 'flags Patch.t
type 'flags pipe
type 'flags region
type side = [ `local | `remote ]
type 'flags observer =
  'flags region -> side -> 'flags patch ->
  (unit -> unit) option

module Pipe :
sig
  type 'msg t = 'msg pipe

  val make : change:('msg -> unit) -> 'msg t
  val commit : 'msg t -> 'msg -> unit
  val status : 'msg t -> [ `Pending | `Connected ]

  (* Fails with Invalid_argument if one of the pipe is closed or connected *)
  val connect : a:'msg t -> b:'msg t -> unit
end

module Region :
sig
  type 'flags t = 'flags region

  val append    : 'flags t -> 'flags list -> string -> unit
  val clear     : 'flags t -> unit
  val kill      : 'flags t -> unit
  val sub       : ?observer:'flags observer -> 'flags t -> 'flags t
  val is_closed : 'flags t -> bool

  val unsafe_left_offset  : 'flags t -> int
  val unsafe_right_offset : 'flags t -> int

  val create : unit -> 'flags t * 'flags patch pipe

  val null : _ region
end
