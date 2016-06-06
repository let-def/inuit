module Patch =
struct
  type 'flags t = {
    offset  : int;
    old_len : int;
    new_len : int;
    text    : string;
    flags   : 'flags list;
  }

  let utf8_length str =
    let count = ref 0 in
    for i = 0 to String.length str - 1 do
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

  let make ~offset ?(replace=0) flags text = {
    offset  = offset;
    old_len = replace;
    new_len = utf8_length text;
    text    = text;
    flags   = flags;
  }
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
    | Pending
    | Connected of 'msg t
    | Closed

  type 'a controller = 'a t

  let make ~receive =
    {
      receive      = receive;
      on_connected = ignore;
      on_closed    = ignore;
      status       = Pending;
    }

  let send t msg =
    match t.status with
    | Connected remote ->
      remote.receive msg
    | Pending ->
      invalid_arg "Inuit.Socket.send: sending data to unconnected pipe"
    | Closed ->
      invalid_arg "Inuit.Socket.send: sending data to closed pipe"

  let close t =
    match t.status with
    | Closed -> ()
    | Pending ->
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
    | Pending, Pending ->
      a.status <- Connected b;
      b.status <- Connected a;
      a.on_connected ();
      b.on_connected ();
      a.on_connected <- ignore;
      b.on_connected <- ignore
    | _ ->
      let to_str = function
        | Pending -> "pending"
        | Closed -> "already closed"
        | Connected _ -> "already connected"
      in
      invalid_arg ("Inuit.Socket.connect: pipe a is " ^ to_str a.status ^
                   "and pipe b is " ^ to_str b.status)

  let status t = match t.status with
    | Pending     -> `Pending
    | Connected _ -> `Connected
    | Closed      -> `Closed

  let set_receive t f = t.receive <- f
  let set_on_closed t f = t.on_closed <- f
  let set_on_connected t f = t.on_connected <- f

  let endpoint socket = socket
end

type 'msg socket = 'msg Socket.t

type side = [ `Local | `Remote ]

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
    mutable closed : bool;
  }

  and 'flags buffer = {
    mutable trope : 'flags t lazy_t Trope.t;
    mutable status : status;
    mutable socket : 'flags patch Socket.controller;
  }

  let unsafe_left_offset  t = Trope.position t.buffer.trope t.left
  let unsafe_right_offset t = Trope.position t.buffer.trope t.right

  let is_open   t =
    not t.closed &&
    (Trope.member t.buffer.trope t.right || (t.closed <- true; false))

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
    exec_observed (notify_observers buffer `Local region ~stop_at:[] patch);
  )

  let region_parent region =
    let lazy parent = region.parent in
    if parent == region then
      None
    else
      Some parent

  let region_before cursor =
    let lazy region = Trope.content cursor in
    if region.right == cursor then
      Some region
    else
      region_parent region

  let region_after cursor =
    let lazy region = Trope.content cursor in
    if region.left == cursor then
      Some region
    else
      region_parent region

  let rec look_for_empty trope position cursor0 = (
    match Trope.cursor_before trope cursor0 with
    | Some cursor when Trope.position trope cursor = position -> (
        let lazy region = Trope.content cursor in
        if region.right == cursor0 then
          Some cursor
        else
          look_for_empty trope position cursor
      )
    | _ -> None
  )

  let insertion_cursor ~left_leaning trope position = (
    match Trope.find_before trope position with
    | None -> (position, None)
    | Some cursor0 ->
      match position - Trope.position trope cursor0 with
      | n when n < 0 -> assert false
      | 0 when left_leaning -> (
          match look_for_empty trope position cursor0 with
          | Some cursor -> (0, Some cursor)
          | None ->
            let lazy region = Trope.content cursor0 in
            if region.left == cursor0 then
              (0, Some cursor0)
            else match Trope.cursor_before trope cursor0 with
              | None -> (position, None)
              | Some cursor ->
                (position - Trope.position trope cursor, Some cursor)
        )
      | n -> (n, Some cursor0)
  )

  let replacement_bound trope position = (
    match Trope.find_after trope position with
    | None -> None
    | Some cursor -> Some (Trope.position trope cursor - position, cursor)
  )

  let ancestor_region l r = (
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
    let trope = b.trope in
    let {Patch. old_len; new_len; offset; _} = patch in
    (* Find bounds *)
    let left_offset, left_cursor = insertion_cursor ~left_leaning:true trope offset in
    let right_bound = replacement_bound trope (offset + old_len) in
    (* Find affected regions and ancestor *)
    let left_region =
      match left_cursor with None -> None | Some c -> region_after c in
    let right_region =
      match right_bound with None -> None | Some (_,c) -> region_before c in
    let ancestor = match left_region, right_region with
      | None, _ | _, None -> None
      | Some l, Some r -> ancestor_region l r
    in
    (* Notify observers *)
    let left_o = match left_region with
      | None -> []
      | Some region ->
        notify_observers b `Remote region ~stop_at:[] patch
    and right_o = match right_region with
        | None -> []
        | Some right ->
          let stop_at = match ancestor with
            | None -> []
            | Some region -> region.observers
          in
          notify_observers b `Remote right ~stop_at patch
    in
    (* Update trope *)
    let trope =
      let trope = match left_cursor, right_bound with
        | Some l, Some (_,r) ->
          Trope.remove_between trope l r
        | Some l, None ->
          Trope.remove_after trope l (left_offset + old_len)
        | None, Some (right_offset,r) ->
          Trope.remove_before trope r (left_offset + old_len + right_offset)
        | None, None ->
          Trope.remove trope ~at:0 ~len:(left_offset + old_len)
      in
      (* Reinsert cursors *)
      let check = match ancestor with
        | None -> (fun _ -> true)
        | Some region -> (!=) region
      in
      let rec reinsert_from_left trope = function
        | Some region when check region ->
          reinsert_from_left
            (Trope.put_back trope region.right) (region_parent region)
        | _ -> trope
      in
      let rec reinsert_from_right trope = function
        | Some region when check region ->
          reinsert_from_right
            (Trope.put_back trope region.left) (region_parent region)
        | _ -> trope
      in
      let trope = reinsert_from_left trope left_region in
      let trope = reinsert_from_right trope right_region in
      (* Fix padding *)
      let trope = match right_bound with
        | None -> trope
        | Some (offset, r) -> Trope.insert_before trope r offset
      in
      let trope = match left_cursor with
        | None -> Trope.insert trope ~at:0 ~len:(left_offset + new_len)
        | Some c -> Trope.insert_after trope c (left_offset + new_len)
      in
      trope
    in
    b.trope <- trope;
    exec_observed right_o;
    exec_observed left_o;
  )

  let remote_insert b patch = (
    let {Patch. new_len; offset; _} = patch in
    let trope = b.trope in
    let left_leaning = new_len > 0 in
    let left_offset, left_cursor =
      insertion_cursor ~left_leaning trope offset in
    let left_region = match left_cursor with
      | None -> None
      | Some cursor -> region_after cursor
    in
    let trope = match left_cursor with
      | None -> Trope.insert trope ~at:left_offset ~len:new_len
      | Some cursor -> Trope.insert_after trope cursor new_len
    in
    let observed = match left_region with
      | None -> []
      | Some region ->
        notify_observers b `Remote region ~stop_at:[] patch
    in
    b.trope <- trope;
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
      let observed = notify_observers buffer `Local t ~stop_at:[] patch in
      buffer.trope <- Trope.insert_before trope t.right patch.Patch.new_len;
      Socket.send buffer.socket patch;
      exec_observed observed;
    )

  let generic_clear f t =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "clear" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.left in
      let length = Trope.position trope t.right - offset in
      let patch = Patch.make ~offset ~replace:length [] "" in
      let observed = notify_observers buffer `Local t ~stop_at:[] patch in
      buffer.trope <- f t buffer.trope;
      Socket.send buffer.socket patch;
      exec_observed observed;
    )

  let clear t =
    generic_clear
      (fun t trope -> Trope.remove_between trope t.left t.right)
      t

  let kill t =
    generic_clear
      (fun t trope ->
         let trope = Trope.remove_between trope t.left t.right in
         let trope = Trope.rem_cursor trope t.left in
         let trope = Trope.rem_cursor trope t.right in
         trope)
      t

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
    let socket = Socket.make ~receive:ignore in
    let rec t' = lazy (
      let trope = Trope.create () in
      let trope, left  = Trope.put_cursor trope ~at:0 t' in
      let trope, right = Trope.put_after  trope left  t' in
      let buffer = { trope; status = Ready; socket } in
      { buffer; left; right; observers = []; parent = t'; closed = false }
    ) in
    let lazy t' = t' in
    let buffer = t'.buffer in
    Socket.set_receive socket (remote_change buffer);
    Socket.set_on_closed socket
      (fun () -> buffer.trope <- Trope.clear buffer.trope);
    (t', Socket.endpoint socket)
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

  let kill = function
    | Null -> ()
    | Concrete t -> Concrete_region.kill t

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
