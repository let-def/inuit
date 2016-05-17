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

module Concrete_region =
struct

  type status =
    | Ready
    | Locked

  type 'flags t = {
    buffer : 'flags buffer;
    left   : 'flags t lazy_t Trope.cursor;
    right  : 'flags t lazy_t Trope.cursor;
    parent : 'flags t lazy_t;
    observers : (side -> 'flags patch -> (unit -> unit) option) lazy_t list;
  }

  and 'flags buffer = {
    mutable trope : 'flags t lazy_t Trope.t;
    mutable status : status;
    pipe : 'flags pipe;
  }

  let unsafe_left_offset  t = Trope.position t.buffer.trope t.left
  let unsafe_right_offset t = Trope.position t.buffer.trope t.right

  let is_open   t = Trope.member t.buffer.trope t.right
  let is_closed t = not (is_open t)

  let notify_observers buffer side region ~stop_at patch = (
    assert (buffer.status = Ready);
    let rec aux acc = function
      | [] -> acc
      | fs when fs == stop_at -> acc
      | lazy f :: fs ->
        let acc = match f side patch with
          | None -> acc
          | Some f' -> f' :: acc
        in
        aux acc fs
    in
    buffer.status <- Locked;
    let fs =
      try aux [] region.observers
      with exn ->
        buffer.status <- Ready;
        raise exn
    in
    buffer.status <- Ready;
    fs
  )

  let exec_observed fs =
    List.iter (fun f -> f ()) fs

  let check_local_change name buffer = (
    match buffer.status with
    | Locked ->
      invalid_arg ("Inuit_base.Region."^name^
                   ": attempt to change locked buffer \
                    (buffer under observation)")
    | Ready -> ()
  )

  let local_change buffer region patch = (
    exec_observed (notify_observers buffer `local region ~stop_at:[] patch);
  )

  let region_parent region =
    let lazy parent = region.parent in
    if parent == region then
      None
    else
      Some parent

  let enclosing_before trope offset = (
    match Trope.find_before trope offset with None -> None | Some cursor ->
      let lazy region = Trope.content cursor in
      if region.left == cursor then
        Some region
      else (
        assert (region.right == cursor);
        region_parent region
      )
  )

  let enclosing_after trope offset = (
    match Trope.find_after trope offset with None -> None | Some cursor ->
      let lazy region = Trope.content cursor in
      if region.right == cursor then
        Some region
      else (
        assert (region.left == cursor);
        region_parent region
      )
  )

  let regions_ancestor l r = (
    let rec aux l r =
      let c = Trope.compare l.left r.left in
      if c < 0 then
        match region_parent r with
        | None -> None
        | Some r' -> aux l r'
      else if c > 0 then
        match region_parent l with
        | None -> None
        | Some l' -> aux l' r
      else
        Some l
    in
    aux l r
  )

  let remote_replace b patch = (
    let {Patch. old_len; new_len; offset; _} = patch in
    let left = enclosing_before b.trope offset in
    let right = enclosing_after b.trope (offset + old_len) in
    let ancestor = match left, right with
      | None, _ | _, None -> None
      | Some l, Some r -> regions_ancestor l r
    in
    (* Notify left branch *)
    let left_o = match left with
      | None -> []
      | Some left -> notify_observers b `remote left ~stop_at:[] patch
    in
    (* Notify right branch *)
    let right_o = match right with
      | None -> []
      | Some right ->
        let stop_at = match ancestor with
          | None -> []
          | Some region -> region.observers
        in
        notify_observers b `remote right ~stop_at patch
    in
    (* Update trope *)
    let trope = Trope.remove ~at:offset ~len:old_len b.trope in
    let trope = Trope.insert trope ~at:offset ~len:new_len in
    b.trope <- trope;
    exec_observed right_o;
    exec_observed left_o;
  )

  let remote_insert b patch = (
    let {Patch. new_len; offset; _} = patch in
    let observed = match enclosing_before b.trope offset with
      | None -> []
      | Some region -> notify_observers b `remote region ~stop_at:[] patch
    in
    b.trope <- Trope.insert ~left_of:() ~at:offset ~len:new_len b.trope;
    exec_observed observed
  )


  let remote_change b patch = (
    match b.status with
    | Locked ->
      invalid_arg "Inuit_base.Region.remote_change: \
                   attempt to change locked buffer \
                   (buffer under observation)"
    | Ready ->
      if patch.Patch.old_len = 0 then
        remote_insert b patch
      else
        remote_replace b patch
  )

  let append t flags text =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "append" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.right in
      let patch = Patch.make ~offset ~replace:0 flags text in
      let observed = notify_observers buffer `local t ~stop_at:[] patch in
      buffer.trope <- Trope.insert_before trope t.right patch.Patch.new_len;
      Pipe.commit buffer.pipe patch;
      exec_observed observed;
    )

  let clear t =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "clear" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.left in
      let length = Trope.position trope t.right - offset in
      let patch = Patch.make ~offset ~replace:length [] "" in
      let observed = notify_observers buffer `local t ~stop_at:[] patch in
      buffer.trope <- Trope.remove_between trope t.left t.right;
      Pipe.commit buffer.pipe patch;
      exec_observed observed;
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
        { t with left; right; observers; parent = lazy t }
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
      { buffer; left; right; observers = []; parent = t' }
    ) in
    let lazy t' = t' in
    t', t'.buffer.pipe
end

module Region =
struct
  type 'flags t =
    | Concrete of 'flags Concrete_region.t
    | Null

  let append t flags text =
    match t with
    | Null -> ()
    | Concrete t -> Concrete_region.append t flags text

  let clear = function
    | Null -> ()
    | Concrete t -> Concrete_region.clear t

  let sub ?observer = function
    | Null -> Null
    | Concrete t ->
      let observer = match observer with
        | None -> None
        | Some f -> Some (fun region -> f (Concrete region))
      in
      Concrete (Concrete_region.sub ?observer t)

  let is_closed = function
    | Null -> true
    | Concrete t -> Concrete_region.is_closed t

  let unsafe_left_offset = function
    | Null -> 0
    | Concrete t -> Concrete_region.unsafe_left_offset t

  let unsafe_right_offset = function
    | Null -> 0
    | Concrete t -> Concrete_region.unsafe_right_offset t

  let create () =
    let region, pipe = Concrete_region.create () in
    Concrete region, pipe

  let null = Null
end

type 'flags region = 'flags Region.t
type 'flags observer =
  'flags region -> side -> 'flags patch -> (unit -> unit) option
