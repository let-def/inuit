open Inuit

type 'a clickable = [> `Clickable | `Clicked ] as 'a

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
      | Some action -> Inuit.action t.cursor action
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
      | None -> Inuit.sub t.cursor
      | Some action -> Inuit.action t.cursor action
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
