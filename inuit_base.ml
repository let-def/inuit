module Patch =
struct
  type 'flags t = {
    offset  : int;
    old_len : int;
    new_len : int;
    text    : string;
    flags   : 'flags list;
  }

  let text_length str =
    let count = ref 0 in
    for i = 0 to String.length str - 1 do
      let c = Char.code str.[i] in
      if c land 0xC0 <> 0x80 then
        incr count
    done;
    !count

  let make ~offset ?(replace=0) flags text = {
    offset  = offset;
    old_len = replace;
    new_len = text_length text;
    text    = text;
    flags   = flags;
  }
end

type 'flags patch = 'flags Patch.t

module Pipe =
struct

  type 'flags status =
    | Connected of ('flags patch -> unit)
    | Pending

  type 'flags t = {
    local  : 'flags patch -> unit;
    mutable status : 'flags status;
  }

  let make ~change:local = {
    local; status = Pending;
  }

  let status t = match t.status with
    | Pending -> `Pending
    | Connected _ -> `Connected

  let connect_check name t =
    match status t with
    | `Connected ->
      invalid_arg ("Inuit.Pipe.connect: "^name^" already connected")
    | `Closed ->
      invalid_arg ("Inuit.Pipe.connect: "^name^" closed")
    | `Pending -> ()

  let connect ~a ~b = (
    if a == b then invalid_arg "Inuit.Pipe.connect: same pipe";
    connect_check "a" a;
    connect_check "b" b;
    a.status <- Connected b.local;
    b.status <- Connected a.local;
  )

  let commit t patch =
    match t.status with
    | Pending -> invalid_arg "Inuit.Pipe.commit: sending data to unconnected pipe"
    | Connected f -> f patch

end

type 'flags pipe = 'flags Pipe.t

type side = [ `local | `remote ]

module Region =
struct

  type status =
    | Ready
    | Locked

  type 'flags t = {
    buffer : 'flags buffer;
    left  : 'flags t lazy_t Trope.cursor;
    right : 'flags t lazy_t Trope.cursor;
    observers : (side -> 'flags patch -> (unit -> unit) option) lazy_t list;
  }

  and 'flags buffer = {
    mutable trope : 'flags t lazy_t Trope.t;
    mutable status : status;
    pipe : 'flags pipe;
  }

  let left_offset  t = Trope.position t.buffer.trope t.left
  let right_offset t = Trope.position t.buffer.trope t.right

  let is_open   t = Trope.member t.buffer.trope t.right
  let is_closed t = not (is_open t)

  let notify_observers buffer side patch = (
    assert (buffer.status = Ready);
    match Trope.find_before buffer.trope patch.Patch.offset with
    | None -> ()
    | Some cursor ->
      let rec aux acc side patch = function
        | [] -> acc
        | lazy f :: fs ->
          let acc = match f side patch with
            | None -> acc
            | Some f' -> f' :: acc
          in
          aux acc side patch fs
      in
      buffer.status <- Locked;
        let lazy {observers} = Trope.content cursor in
        let fs =
          try aux [] side patch observers
          with exn ->
            buffer.status <- Ready;
            raise exn
        in
        buffer.status <- Ready;
        List.iter (fun f -> f ()) fs;
  )

  let check_local_change name buffer = (
    match buffer.status with
    | Locked ->
      invalid_arg ("Inuit_base.Region."^name^
                   ": attempt to change locked buffer \
                    (buffer under observation)")
    | Ready -> ()
  )

  let local_change buffer patch = (
    notify_observers buffer `local patch;
    Pipe.commit buffer.pipe patch;
  )

  let remote_change b patch = (
    match b.status with
    | Locked ->
      invalid_arg "Inuit_base.Region.remote_change: \
                   attempt to change locked buffer \
                   (buffer under observation)"
    | Ready ->
      let open Patch in
      if patch.old_len <> 0 then
        b.trope <- Trope.remove ~at:patch.offset ~len:patch.old_len b.trope;
      if patch.new_len <> 0 then
        b.trope <- Trope.insert ~at:patch.offset ~len:patch.new_len b.trope;
      notify_observers b `remote patch
  )

  let append t flags text =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "append" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.right in
      let patch = Patch.make ~offset ~replace:0 flags text in
      buffer.trope <- Trope.insert_before trope t.right patch.Patch.new_len;
      local_change buffer patch;
    )

  let clear t =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "clear" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.left in
      let length = Trope.position trope t.right - offset in
      buffer.trope <- Trope.remove_between trope t.left t.right;
      let patch = Patch.make ~offset ~replace:length [] "" in
      local_change buffer patch;
    )

  let sub ?observer t =
    if is_open t then
      let rec t' = lazy (
        let trope = t.buffer.trope in
        let trope, left  = Trope.put_before trope t.right t' in
        let trope, right = Trope.put_after  trope left t' in
        t.buffer.trope <- trope;
        let observers = match observer with
          | None -> t.observers
          | Some observer -> lazy (observer (Lazy.force t')) :: t.observers
        in
        { t with left; right; observers }
      ) in
      let lazy t' = t' in
      begin match t'.observers with
        | [] -> ()
        | lazy _x :: _ -> ()
      end;
      t'

    else t

  let create () =
    let rec t' = lazy (
      let trope = Trope.create () in
      let trope, left  = Trope.put_cursor trope ~at:0 t' in
      let trope, right = Trope.put_after  trope left  t' in
      let rec buffer = {
        trope;
        status = Ready;
        pipe = { Pipe. local; status = Pipe.Pending };
      }
      and local patch = remote_change buffer patch in
      { buffer; left; right; observers = []; }
    ) in
    Lazy.force t'

  let pipe t = t.buffer.pipe
end

type 'flags region = 'flags Region.t
type 'flags observer =
  'flags region -> side -> 'flags patch -> (unit -> unit) option
