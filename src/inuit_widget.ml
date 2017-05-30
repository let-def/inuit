open Inuit_base
open Inuit_cursor

type 'a clickable = [> `Clickable | `Clicked ] as 'a
type 'a editable = [> `Editable | `Prompt ] as 'a

module Nav =
struct
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
    (*⏪*) (*↻*) (*⏩*)
    link cursor "[<<]" (fun _ -> prev t);
    text cursor " ";
    link cursor "[reload]" (fun _ -> update_frame t);
    text cursor " ";
    link cursor	"[>>]" (fun _ -> next t)

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

module Tree =
struct
  type 'flags t = 'flags cursor

  let not_closed t =
    not (is_closed t)

  let make cursor = sub cursor

  let add_leaf ?action t =
    text t "\n";
    match action with
    | Some action -> clickable t action
    | None -> sub t

  let add_node children ?action ?(opened=ref false) t =
    let body = ref None in
    text t "\n";
    link t (if !opened then "[-]" else "[+]") (fun c ->
        match !body with
        | None -> ()
        | Some t' when !opened ->
          opened := false;
          clear c; text c "[+]";
          clear t'
        | Some t' ->
          opened := true;
          clear c; text c "[-]";
          ignore (children t')
      );
    text t " ";
    let result = match action with
      | None -> sub t
      | Some action -> clickable t action
    in
    let t' = shift_indent (sub t) (+1) in
    body := Some t';
    let acc = if !opened then Some (children t') else None in
    (acc, result)

  let add ?children ?action ?opened t =
    if not_closed t then (
      match children with
      | None -> add_leaf ?action t
      | Some children -> snd (add_node children ?action ?opened t)
    ) else
      t

  let add_and_return ~children ?action ?opened t =
    if not_closed t then (
      match add_node children ?action ?opened t with
      | None, result -> (children null, result)
      | Some acc, result -> (acc, result)
    ) else
      (children null, t)

  let clear t = clear t

  let cursor t = t

  let null = null
end

module Check =
struct
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

module Edit =
struct
  type 'flags t = {
    mutable cursor: 'flags cursor;
    mutable state: string;
  }

  let make ?(state="") ?on_change cursor = (
    let t = { cursor; state = "" } in
    let on_change = match on_change with
      | None -> None
      | Some f -> Some (fun _ -> f t)
    in
    text (add_flag `Prompt cursor) "# ";
    t.cursor <- observe cursor
        (fun cursor' side p ->
           let callback =
             let offset = Inuit_region.unsafe_left_offset (region cursor') in
             match Patch.utf8_offset t.state (p.Patch.offset - offset) with
             | exception Not_found ->
               None
             | offset ->
               let sl = String.sub t.state 0 offset in
               let sr =
                 match Patch.utf8_offset t.state ~offset (Patch.removed p) with
                 | exception Not_found -> ""
                 | offset ->
                   String.sub t.state offset (String.length t.state - offset)
               in
               t.state <- sl ^ Patch.inserted_text p ^ sr;
               if side = `Remote then
                 on_change
               else None
           in
           (p.Patch.flags, callback)
        );
    text t.cursor state;
  )

  let change t ~state =
    clear t.cursor;
    text t.cursor state

  let state t = t.state
end

module Slider =
struct
  let prepare_editable ~prompt cursor = (
    let cursor = add_flag `Editable cursor in
    let rubber = observe cursor
        (fun cursor' side p ->
           let {Patch. flags} = p in
           (flags,
            if side = `Local then
              None
            else
              Some (fun _ ->
                  clear cursor';
                  text cursor' prompt;
                ))
        )
    in
    text rubber prompt;
    cursor
  )

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
           p.Patch.flags,
           if side = `Remote then (
             let delta = Patch.inserted p - Patch.removed p
                         - count_chars (Patch.inserted_text p) '-' * 2 in
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
