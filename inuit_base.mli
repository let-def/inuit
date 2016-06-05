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

module Pipe :
sig
  type 'msg t

  val setup : unit ->
    'msg t * (on_connected:('msg t -> unit) -> on_closed:(unit -> unit) ->
              receive:('msg -> unit) -> unit)

  val make :
    ?on_connected:('msg t -> unit) ->
    ?on_closed:(unit -> unit) ->
    ('msg -> unit) -> 'msg t

  val send : 'msg t -> 'msg -> unit
  val close : 'msg t -> unit
  val status : 'msg t -> [ `Pending | `Connected | `Closed ]

  (* Fails with Invalid_argument if one of the pipe is pending or closed *)
  val connect : a:'msg t -> b:'msg t -> unit
end

type 'flags patch = 'flags Patch.t
type 'msg pipe = 'msg Pipe.t
type 'flags region
type side = [ `local | `remote ]
type 'flags observer =
  'flags region -> side -> 'flags patch ->
  (unit -> unit) option

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
