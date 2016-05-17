open Inuit
open Inuit.Cursor

type 'a clickable = [> `Clickable | `Clicked ] as 'a
type 'a editable = [> `Editable ] as 'a

module Nav = struct

  type 'flags t = {
    mutable prev: 'flags page list;
    mutable page: 'flags page;
    mutable next: 'flags page list;

    frame: 'flags frame option;
  }

  and 'flags page = string * ('flags frame -> unit)

  and 'flags frame = {
    title: 'flags cursor;
    body: 'flags cursor;
    nav: 'flags t;
  }

  let null_page : _ page = "", ignore

  let make title body =
    let page = (title, body) in
    { prev = []; page; next = []; frame = None }

  let update_frame t = match t.frame with None -> () | Some frame ->
    let {title; body} = frame in
    clear title;
    text title (fst t.page);
    clear body;
    (snd t.page) frame

  let goto t title body =
    t.page <- (title, body);
    t.next <- [];
    update_frame t

  let push t title body =
    t.prev <- t.page :: t.prev;
    goto t title body

  let next t = match t.next with
    | [] -> ()
    | page :: pages ->
      t.prev <- t.page :: t.prev;
      t.page <- page;
      t.next <- pages;
      update_frame t

  let prev t = match t.prev with
    | [] -> ()
    | page :: pages ->
      t.next <- t.page :: t.next;
      t.page <- page;
      t.prev <- pages;
      update_frame t

  let render_header t cursor =
    link cursor "⏪" (fun _ -> prev t);
    text cursor " ";
    link cursor "↻" (fun _ -> update_frame t);
    text cursor " ";
    link cursor	"⏩" (fun _ -> next t)

  let render t cursor =
    if not (is_closed cursor) then (
      let header = sub cursor in
      text cursor " ";
      let title = sub cursor in
      text cursor "\n\n";
      let body = sub cursor in
      let rec nav = {t with frame = Some frame}
      and frame = { title = title; body = body; nav = nav }
      in
      render_header nav header;
      update_frame nav
    )
end

module Tree = struct

  type 'flags t = {
    indent: int;
    cursor: 'flags cursor;
  }

  let not_closed t =
    not (is_closed t.cursor)

  let make cursor =
    { indent = 0; cursor = sub cursor }

  let indent t =
    if t.indent > 0 then
      text t.cursor (String.make t.indent ' ')

  let add_leaf ?action t =
    indent t;
    (*text t.cursor "  ";*)
    let result = match action with
      | Some action -> clickable t.cursor action
      | None -> sub t.cursor
    in
    text t.cursor "\n";
    result

  let add_node children ?action ?(opened=ref false) t =
    indent t;
    let body = ref None in
    link t.cursor (if !opened then "▪" else "▫") (fun c ->
        match !body with
        | None -> ()
        | Some t' when !opened ->
          opened := false;
          clear c; text c "▫";
          clear t'.cursor
        | Some t' ->
          opened := true;
          clear c; text c "▪";
          children t'
      );
    text t.cursor " ";
    let result = match action with
      | None -> sub t.cursor
      | Some action -> clickable t.cursor action
    in
    text t.cursor "\n";
    let t' = { indent = t.indent + 1; cursor = sub t.cursor } in
    body := Some t';
    if !opened then children t';
    result

  let add ?children ?action ?opened t =
    if not_closed t then (
      match children with
      | None -> add_leaf ?action t
      | Some children -> add_node children ?action ?opened t
    ) else
      t.cursor

  let clear t = clear t.cursor
end

module Check = struct
  type 'flags t = {
    mutable cursor: 'flags cursor;
    mutable state: bool;
  }

  let render t = (
    clear t.cursor;
    text t.cursor (if t.state then "x" else "_")
  )

  let make ?(state=false) ?(on_change=ignore) cursor = (
    text cursor "[";
    let t = { cursor; state } in
    t.cursor <- clickable cursor (fun check ->
        t.state <- not t.state;
        render t;
        on_change t
      );
    render t;
    text cursor "]";
    t
  )

  let change t ~state =
    t.state <- state;
    render t

  let state t = t.state
end

let prepare_editable ~prompt cursor = (
  let cursor = add_flag `Editable cursor in
  let rubber = observe cursor
      (fun cursor' side p ->
         if side = `local then None
         else Some (fun _ ->
             clear cursor';
             text cursor' prompt;
           )
      )
  in
  text rubber prompt;
  cursor
)

module Edit = struct
  type 'flags t = {
    mutable cursor: 'flags cursor;
    mutable state: string;
  }

  let render t = (
    clear t.cursor;
    text t.cursor t.state;
  )

  let make ?(state="") ?on_change cursor = (
    let t = { cursor; state } in
    let on_change = match on_change with
      | None -> None
      | Some f -> Some (fun _ -> f t)
    in
    text cursor "[";
    t.cursor <- observe (prepare_editable ~prompt:"|" cursor)
        (fun cursor' side p ->
           let offset = Region.unsafe_left_offset (region cursor') in
           let delta = p.Patch.offset - offset in
           let sl = String.sub t.state 0 delta in
           let offset = delta + p.Patch.old_len in
           let sr = String.sub t.state offset (String.length t.state - offset)  in
           t.state <- sl ^ p.Patch.text ^ sr;
           if side = `remote then
             on_change
           else None
        );
    text cursor "|]";
    render t;
  )

  let change t ~state =
    t.state <- state;
    render t

  let state t = t.state
end

module Slider =
struct
  type 'flags t = {
    mutable cursor : 'flags cursor;
    mutable state : (int * int);
  }

  let render t = (
    let str =
      match t.state with
      | (0, max) -> String.make max '-'
      | (pos, max) when pos >= max -> String.make max '#'
      | (pos, max) -> String.make pos '#' ^ String.make (max - pos) '-'
    in
    clear t.cursor;
    text t.cursor str;
  )

  let count_chars str c =
    let count = ref 0 in
    for i = 0 to String.length str - 1 do
      if str.[i] = c then incr count
    done;
    !count

  let make ?(state = (0, 20)) ?on_change cursor = (
    let t = { cursor; state } in
    t.cursor <- observe (prepare_editable ~prompt:"|" cursor)
        (let update = match on_change with
            | None -> Some (fun _ -> render t)
            | Some f -> Some (fun _ -> render t; f t)
         in fun cursor' side p ->
           if side = `remote then (
             let delta =
               p.Patch.new_len
               - p.Patch.old_len
               - count_chars p.Patch.text '-' * 2
             in
             let pos, max = t.state in
             let pos = pos + delta in
             let pos = if pos < 0 then 0 else if pos > max then max else pos in
             t.state <- (pos, max);
             update
           ) else None
        );
    text cursor "|";
    render t;
  )

  let change t ~state =
    t.state <- state;
    render t

  let state t = t.state
end
