open Inuit_base

type revision = {
  remote: int;
  local: int;
}

type 'flag remote_patch =
  | Ack of revision
  | Patch of revision * 'flag patch

type op =
  | R of int * int
  | I of int * int

type version_control = {
  mutable remote_local: int;
  mutable latest_remote_sync: int;

  mutable local_local: int;
  mutable revisions: (int * op) list;
  mutable rev_tail: (int * op) list;
}

type 'flag state = {
  remote_socket : 'flag remote_patch Socket.controller;
  local_socket  : 'flag patch Socket.controller;
  version : version_control;
}

let current_revision t = { remote = t.remote_local; local = t.local_local }

let old_len p = match p.Patch.operation with
  | Patch.Remove n | Patch.Replace (n,_) -> n
  | _ -> 0

let new_len p = match p.Patch.operation with
  | Patch.Insert _ | Patch.Replace _ -> p.Patch.text_len
  | _ -> 0

let remote_lift_patch t p =
  let open Patch in
  t.local_local <- t.local_local + 1;
  let old_len = old_len p in
  let new_len = new_len p in
  if old_len <> 0 then
    t.revisions <- (t.local_local, R (p.offset, old_len)) :: t.revisions;
  if new_len <> 0 then
    t.revisions <- (t.local_local, I (p.offset, new_len)) :: t.revisions;
  Patch (current_revision t, p)

let local_to_remote t patch =
  let patch = remote_lift_patch t.version patch in
  t.version.latest_remote_sync <- t.version.remote_local;
  Socket.send t.remote_socket patch

let update_remote_revision t remote =
  let version = t.version in
  version.remote_local <- remote.local;
  let rec filter = function
    | (local, _) :: xs when local <= remote.remote -> filter xs
    | xs -> xs
  in
  let rec rev_filter acc = function
    | (local, _) as x :: xs when local > remote.remote ->
      rev_filter (x :: acc) xs
    | _ -> acc
  in
  begin match filter version.rev_tail with
    | [] ->
      version.rev_tail <- rev_filter [] version.revisions;
      version.revisions <- []
    | xs ->
      version.rev_tail <- xs
  end;
  if version.latest_remote_sync < remote.local - 16 then (
    t.version.latest_remote_sync <- remote.local;
    Socket.send t.remote_socket (Ack (current_revision version))
  )

let commute_remove (_,op') (s2, l2) = match op' with
  | R (s1, l1) ->
    let remap x =
      if x < s1 then
        x
      else if x > s1 + l1 then
        x - l1
      else s1
    in
    let e2 = s2 + l2 in
    let s2 = remap s2 and e2 = remap e2 in
    if e2 = s2 then
      raise Not_found
    else (s2, e2 - s2)
  | I (s1, l1) ->
    if s1 < s2 then
      (s2 + l1, l2)
    else if s1 >= s2 + l2 then
      (s2, l2)
    else
      (s2, l2 + l1)

let commute_point lean (_,op') s2 = match op' with
  | R (s1, l1) ->
    if s2 < s1 then
      s2
    else if s2 >= s1 + l1 then
      (s2 - l1)
    else (match lean with
      | `Left -> s1
      | `None -> raise Not_found
      | `Right -> s1 + l1
      )
  | I (s1, l1) ->
    if s1 <= s2 then
      (s2 + l1)
    else
      s2

let commute_remote_op t op_kind op_arg =
  let flip f x y = f y x in
  let op_arg = List.fold_left (flip op_kind) op_arg t.rev_tail in
  let op_arg = List.fold_right op_kind t.revisions op_arg in
  op_arg

let remote_to_local t = function
  | Ack revision -> update_remote_revision t revision
  | Patch (revision, p) ->
    update_remote_revision t revision;
    let o = p.Patch.offset in
    begin match
      match p.Patch.operation with
      | Patch.Insert text ->
          let o = commute_remote_op t.version (commute_point `None) o in
          (o, Patch.Insert text)
      | Patch.Propertize n ->
          let l = commute_remote_op t.version (commute_point `Right) o in
          let r = commute_remote_op t.version (commute_point `Left) (o+n) in
          let n' = r - l in
          if n' < 0 then raise Not_found;
          (l, Patch.Propertize n')
      | Patch.Remove n ->
          let o, n' = commute_remote_op t.version commute_remove (o, n) in
          (o, Patch.Remove n')
      | Patch.Replace (n, text) ->
          let o, n' = commute_remote_op t.version commute_remove (o, n) in
          (o, Patch.Replace (n', text))
      with
      | exception Not_found -> ()
      | (offset, op) ->
          Socket.send t.local_socket (Patch.make ~offset p.Patch.flags op)
    end

let make () =
  let t = {
    remote_socket = Socket.make ~receive:ignore;
    local_socket  = Socket.make ~receive:ignore;
    version = {
      revisions = []; rev_tail = [];
      local_local  = 0;
      remote_local = 0;
      latest_remote_sync = 0;
    }
  } in
  let on_closed () =
    Socket.close t.remote_socket;
    Socket.close t.local_socket
  in
  Socket.set_on_closed t.remote_socket on_closed;
  Socket.set_on_closed t.local_socket on_closed;
  Socket.set_receive t.local_socket (local_to_remote t);
  Socket.set_receive t.remote_socket (remote_to_local t);
  (Socket.endpoint t.remote_socket, Socket.endpoint t.local_socket)
