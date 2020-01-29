module Patch =
struct

  type operation =
    | Remove  of int
    | Insert  of string
    | Replace of int * string
    | Propertize of int

  type 'flags t =
    { offset    : int         (** Starting at [offset]'th unicode sequence *)
    ; operation : operation
    ; text_len  : int
    ; flags     : 'flags list (** A list of backend defined [flags].       *)
    }

  let utf8_length ?(offset=0) str =
    let count = ref 0 in
    for i = offset to String.length str - 1 do
      let c = Char.code str.[i] in
      if c land 0xC0 <> 0x80 then
        incr count
    done;
    !count

  let utf8_offset str ?(offset=0) index =
    let index = ref index and offset = ref offset and len = String.length str in
    while !index > 0 && !offset < len do
      incr offset;
      decr index;
      while !offset < len && (Char.code str.[!offset] land 0xC0 = 0x80) do
        incr offset;
      done;
    done;
    if !index > 0 then raise Not_found;
    !offset

  let make ~offset flags operation = {
    flags = flags; offset = offset; operation;
    text_len = (match operation with
      | Insert text | Replace (_,text) -> utf8_length text
      | _ -> 0
      );
  }

  let with_flags flags t =
    if t.flags == flags then t else {t with flags}

  let removed t = match t.operation with
    | Insert _ | Propertize _ -> 0
    | Remove n | Replace (n,_) -> n

  let inserted t = match t.operation with
    | Insert _ | Replace _ -> t.text_len
    | Propertize _ | Remove _ -> 0

  let inserted_text t = match t.operation with
    | Insert txt | Replace (_,txt) -> txt
    | Propertize _ | Remove _ -> ""
end

type 'flags patch = 'flags Patch.t

module Socket =
struct

  type 'msg t = {
    mutable receive      : 'msg -> unit;
    mutable on_connected : unit -> unit;
    mutable on_closed    : unit -> unit;
    mutable status       : 'msg status;
  }

  and 'msg status =
    | Pending of 'msg list
    | Connected of 'msg t
    | Closed

  type 'a controller = 'a t

  let make ~receive =
    {
      receive      = receive;
      on_connected = ignore;
      on_closed    = ignore;
      status       = Pending [];
    }

  let send ?(buffer=true) t msg =
    match t.status with
    | Connected remote ->
      remote.receive msg
    | Pending msgs ->
      if buffer then
        t.status <- Pending (msg :: msgs)
      else
        invalid_arg "Inuit.Socket.send: sending data to unconnected pipe"
    | Closed ->
      invalid_arg "Inuit.Socket.send: sending data to closed pipe"

  let close t =
    match t.status with
    | Closed -> ()
    | Pending _drop ->
      (* TODO: do something with dropped messages? *)
      t.status <- Closed;
      t.on_closed ();
      t.on_connected <- ignore;
      t.receive <- ignore;
      t.on_closed <- ignore;
    | Connected remote ->
      t.status <- Closed;
      remote.status <- Closed;
      t.on_closed ();
      remote.on_closed ();
      t.receive <- ignore;
      t.on_closed <- ignore;
      remote.receive <- ignore;
      remote.on_closed <- ignore

  let connect ~a ~b =
    match a.status, b.status with
    | Pending bmsgs, Pending amsgs ->
      a.status <- Connected b;
      b.status <- Connected a;
      a.on_connected ();
      b.on_connected ();
      a.on_connected <- ignore;
      b.on_connected <- ignore;
      List.iter b.receive (List.rev bmsgs);
      List.iter a.receive (List.rev amsgs)
    | _ ->
      let to_str = function
        | Pending _ -> "pending"
        | Closed -> "already closed"
        | Connected _ -> "already connected"
      in
      invalid_arg ("Inuit.Socket.connect: pipe a is " ^ to_str a.status ^
                   "and pipe b is " ^ to_str b.status)

  let status t = match t.status with
    | Pending _   -> `Pending
    | Connected _ -> `Connected
    | Closed      -> `Closed

  let set_receive t f = t.receive <- f
  let set_on_closed t f = t.on_closed <- f
  let set_on_connected t f = t.on_connected <- f

  let endpoint socket = socket
end

type 'msg socket = 'msg Socket.t
