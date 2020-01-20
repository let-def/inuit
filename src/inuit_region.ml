open Inuit_base

type side = [ `Local | `Remote ]

let cons_some x xs = match x with
  | None -> xs
  | Some x -> x :: xs

module Concrete =
struct

  type status =
    | Ready
    | Locked

  type 'flags t = {
    buffer : 'flags buffer;
    left   : Trope.cursor;
    right  : Trope.cursor;
    parent : 'flags t;
    observers : (side -> 'flags patch -> 'flags list * (unit -> unit) option) lazy_t list;
    mutable closed : bool;
  }

  and 'flags buffer = {
    mutable trope : 'flags t Trope.t;
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
    let rec aux patch acc = function
      | [] -> acc
      | fs when fs == stop_at -> acc
      | lazy f :: fs ->
        let flags, f' = f side patch in
        let patch = Patch.with_flags flags patch in
        let acc = cons_some f' acc in
        aux patch acc fs
    in
    buffer.status <- Locked;
    let fs =
      try aux patch [] region.observers
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

  let region_parent region =
    let parent = region.parent in
    if parent == region then
      None
    else
      Some parent

  let region_before trope cursor =
    match Trope.find trope cursor with
    | region when region.right == cursor -> Some region
    | region -> region_parent region
    | exception Not_found -> None

  let region_after trope cursor =
    match Trope.find trope cursor with
    | region when region.left == cursor -> Some region
    | region -> region_parent region
    | exception Not_found -> None

  let rec look_for_empty trope position cursor0 = (
    match Trope.seek_before trope cursor0 with
    | Some (cursor, region) when Trope.position trope cursor = position -> (
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
    | Some (cursor0, region) ->
      match position - Trope.position trope cursor0 with
      | n when n < 0 -> assert false
      | 0 when left_leaning -> (
          match look_for_empty trope position cursor0 with
          | Some cursor -> (0, Some cursor)
          | None ->
            if region.left == cursor0 then
              (0, Some cursor0)
            else match Trope.seek_before trope cursor0 with
              | None -> (position, None)
              | Some (cursor, _) ->
                (position - Trope.position trope cursor, Some cursor)
        )
      | n -> (n, Some cursor0)
  )

  let replacement_bound trope position = (
    match Trope.find_after trope position with
    | None -> None
    | Some (cursor, _region) ->
      Some (Trope.position trope cursor - position, cursor)
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

  let remote_replace b ({Patch.offset; _} as patch) old_len new_len = (
    let trope = b.trope in
    (* Find bounds *)
    let left_offset, left_cursor =
      insertion_cursor ~left_leaning:true trope offset in
    let right_bound = replacement_bound trope (offset + old_len) in
    (* Find affected regions and ancestor *)
    let left_region =
      match left_cursor with None -> None | Some c -> region_after trope c in
    let right_region =
      match right_bound with None -> None | Some (_,c) -> region_before trope c in
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
            (Trope.put_left trope region.right region) (region_parent region)
        | _ -> trope
      in
      let rec reinsert_from_right trope = function
        | Some region when check region ->
          reinsert_from_right
            (Trope.put_left trope region.left region) (region_parent region)
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

  let remote_propertize b ({Patch.offset; _} as patch) len = (
    let trope = b.trope in
    (* Find bounds *)
    let _left_offset, left_cursor =
      insertion_cursor ~left_leaning:false trope offset in
    let right_bound = replacement_bound trope (offset + len) in
    (* Find affected regions and ancestor *)
    let left_region =
      match left_cursor with None -> None | Some c -> region_after trope c in
    let right_region =
      match right_bound with None -> None | Some (_,c) -> region_before trope c in
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
    exec_observed right_o;
    exec_observed left_o;
  )

  let remote_insert b ({Patch.offset; _} as patch) new_len = (
    let trope = b.trope in
    let left_offset, left_cursor =
      insertion_cursor ~left_leaning:true trope offset in
    let left_region = match left_cursor with
      | None -> None
      | Some cursor -> region_after trope cursor
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
      let {Patch. operation; offset=_; text_len; flags=_} = patch in
        match operation with
        | Patch.Remove n -> remote_replace b patch n 0
        | Patch.Replace (n,_) -> remote_replace b patch n text_len
        | Patch.Insert _ -> remote_insert b patch text_len
        | Patch.Propertize n -> remote_propertize b patch n
  )

  let append t flags text =
    if is_open t then (
      let buffer = t.buffer in
      check_local_change "append" buffer;
      let trope = buffer.trope in
      let offset = Trope.position trope t.right in
      let patch = Patch.make ~offset flags (Patch.Insert text) in
      let observed = notify_observers buffer `Local t ~stop_at:[] patch in
      buffer.trope <- Trope.insert_before trope t.right patch.Patch.text_len;
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
      let patch = Patch.make ~offset [] (Patch.Remove length) in
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

  let propertize flags t =
    if is_open t then (
      let buffer = t.buffer in
      let trope = buffer.trope in
      let offset = Trope.position trope t.left in
      let length = Trope.position trope t.right - offset in
      let patch = Patch.make ~offset flags (Patch.Propertize length) in
      let observed = notify_observers buffer `Local t ~stop_at:[] patch in
      Socket.send buffer.socket patch;
      exec_observed observed;
    )

  let sub ?(at=`Right) ?observer parent =
    if is_open parent then
      let left = match at with
        | `Before -> Trope.cursor_before parent.left
        | `Left   -> Trope.cursor_after parent.left
        | `Right  -> Trope.cursor_before parent.right
        | `After  -> Trope.cursor_after parent.right
      in
      let right = Trope.cursor_after left in
      let parent = match at with
        | `Before | `After -> parent.parent
        | `Left   | `Right -> parent
      in
      let buffer = parent.buffer in
      let t' = match observer with
        | None -> { left; right; parent; buffer; closed = false;
                    observers = parent.observers }
        | Some observer ->
          let rec t' =
            { left; right; parent; buffer; closed = false;
              observers = lazy (observer t') :: parent.observers }
          in
          t'
      in
      let trope = buffer.trope in
      let trope = match at with
        | `Right | `Before -> Trope.put_right trope left t'
        | `Left  | `After  -> Trope.put_left trope left t'
      in
      buffer.trope <- (Trope.put_left trope right t');
      begin match t'.observers with
        | [] -> ()
        | lazy _x :: _ -> ()
      end;
      t'

    else parent

  let make () =
    let socket = Socket.make ~receive:ignore in
    let trope = Trope.create () in
    let left = Trope.cursor_at_origin trope in
    let right = Trope.cursor_after left in
    let rec t' = {
      left;
      right;
      buffer = { trope; status = Ready; socket };
      closed = false;
      parent = t';
      observers = [];
    } in
    let buffer = t'.buffer in
    buffer.trope <- Trope.put_left (Trope.put_left trope left t') right t';
    Socket.set_receive socket (remote_change buffer);
    Socket.set_on_closed socket
      (fun () -> buffer.trope <- Trope.clear buffer.trope);
    (t', Socket.endpoint socket)
end

type 'flags t =
  | Concrete of 'flags Concrete.t
  | Null

let append t flags text =
  match t with
  | Null -> ()
  | Concrete t -> Concrete.append t flags text

let clear = function
  | Null -> ()
  | Concrete t -> Concrete.clear t

let kill = function
  | Null -> ()
  | Concrete t -> Concrete.kill t

let propertize flags = function
  | Null -> ()
  | Concrete t -> Concrete.propertize flags t

let sub ?at ?observer = function
  | Null -> Null
  | Concrete t ->
    let observer = match observer with
      | None -> None
      | Some f -> Some (fun region -> f (Concrete region))
    in
    Concrete (Concrete.sub ?at ?observer t)

let is_closed = function
  | Null -> true
  | Concrete t -> Concrete.is_closed t

let unsafe_left_offset = function
  | Null -> 0
  | Concrete t -> Concrete.unsafe_left_offset t

let unsafe_right_offset = function
  | Null -> 0
  | Concrete t -> Concrete.unsafe_right_offset t

let make () =
  let region, pipe = Concrete.make () in
  Concrete region, pipe

let null = Null

type 'flags observer =
  'flags t -> side -> 'flags patch -> 'flags list * (unit -> unit) option
