external reraise : exn -> 'a = "%reraise"

module Make (M : sig type flag end) :
sig
  type cursor = M.flag Inuit.cursor
  val formatter_of_cursor : cursor -> Format.formatter

  val push_cursor : (cursor -> cursor) -> Format.formatter -> unit
  val push_flags : (M.flag list -> M.flag list) -> Format.formatter -> unit
  val pop : Format.formatter -> unit

  val with_cursor :
    (cursor -> cursor) ->
    (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a -> 'b
  val with_flags :
    (M.flag list -> M.flag list) ->
    (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a -> 'b
end = struct
  type cursor = M.flag Inuit.cursor

  let magic_tag = "inuit-cookie"

  type magic_cookie =
    | Empty
    | Cursor of (cursor -> cursor)
    | Flags of (M.flag list -> M.flag list)

  let magic_cookie = ref Empty

  type state = {
    mutable mark_head: cursor list;
    mutable mark_tail: cursor list;
    mutable print_stack: cursor list;
  }

  let formatter_of_cursor c =
    let state =
      { mark_head = [Inuit_cursor.sub c]; mark_tail = []; print_stack = [c] }
    in
    let pp = Format.make_formatter
        (fun txt ofs len ->
           let txt =
             if ofs = 0 && len = String.length txt
             then txt
             else String.sub txt ofs len
           in
           match state.mark_head with
           | [] -> assert false
           | c' :: cs ->
             let c = match state.print_stack with
               | [c] when Inuit.Cursor.is_closed c' ->
                 let c = Inuit.Cursor.sub c in
                 state.mark_head <- c :: cs;
                 c
               | _ -> c'
             in
             Inuit.Cursor.text c txt
        )
        (fun () -> ())
    in
    let pop_tag tag =
      if tag == magic_tag then (
        match state.mark_head with
        | [] -> assert false
        | [_] ->
          state.mark_head <- List.rev state.mark_tail;
          state.mark_tail <- []
        | _ :: xs -> state.mark_head <- xs;
      );
      ""
    in
    Format.pp_set_formatter_tag_functions pp {
      Format.
      mark_open_tag = pop_tag;
      mark_close_tag = pop_tag;
      print_open_tag = (fun tag -> if tag == magic_tag then (
          match !magic_cookie with
          | Empty -> invalid_arg "Inuit_format.print_open_tag: handler not found (internal error?)"
          | Cursor f ->
            magic_cookie := Empty;
            begin match state.print_stack with
            | [] -> invalid_arg "Inuit_format.print_open_tag: stack is empty (internal error?)"
            | x :: _ ->
              let x = f x in
              state.print_stack <- x :: state.print_stack;
              state.mark_tail <- Inuit.Cursor.sub x :: state.mark_tail
            end
          | Flags f ->
            magic_cookie := Empty;
            begin match state.print_stack with
            | [] -> invalid_arg "Inuit_format.print_open_tag: stack is empty (internal error?)"
            | x :: _ ->
              let x = Inuit.Cursor.with_flags (f (Inuit.Cursor.get_flags x)) x in
              state.print_stack <- x :: state.print_stack;
              state.mark_tail <- Inuit.Cursor.sub x :: state.mark_tail
            end
        ));
      print_close_tag = (fun tag -> if tag == magic_tag then (
          match state.print_stack with
          | _ :: (x :: _ as xs)  ->
            state.print_stack <- xs;
            state.mark_tail <- Inuit.Cursor.sub x :: state.mark_tail;
          | [_] | [] -> invalid_arg "Inuit_format.print_close_tag: unbalanced tag"
        ));
    };
    Format.pp_set_tags pp true;
    pp

  let push_cursor f pp =
    let magic_cookie' = !magic_cookie in
    magic_cookie := Cursor f;
    match Format.pp_open_tag pp magic_tag with
    | () ->
      begin
        let magic_cookie'' = !magic_cookie in
        magic_cookie := magic_cookie';
        match magic_cookie'' with
        | Empty -> ()
        | _ -> invalid_arg "Inuit_format.push_cursor: not a cursor formatter"
      end
    | exception exn ->
      magic_cookie := magic_cookie';
      reraise exn

  let push_flags f pp =
    let magic_cookie' = !magic_cookie in
    magic_cookie := Flags f;
    match Format.pp_open_tag pp magic_tag with
    | () ->
      begin
        let magic_cookie'' = !magic_cookie in
        magic_cookie := magic_cookie';
        match magic_cookie'' with
        | Empty -> ()
        | _ -> invalid_arg "Inuit_format.push_flags: not a cursor formatter"
      end
    | exception exn ->
      magic_cookie := magic_cookie';
      reraise exn

  let pop pp =
    Format.pp_close_tag pp ()

  let with_cursor f k pp x =
    push_cursor f pp;
    match k pp x with
    | result ->
      pop pp;
      result
    | exception exn ->
      pop pp;
      reraise exn

  let with_flags f k pp x =
    push_flags f pp;
    match k pp x with
    | result ->
      pop pp;
      result
    | exception exn ->
      pop pp;
      reraise exn

  let null_formatter =
    let pp = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
    Format.pp_set_mark_tags pp false;
    Format.pp_set_print_tags pp true;
    Format.pp_set_formatter_tag_functions pp {
      Format.
      mark_open_tag = (fun _ -> "");
      mark_close_tag = (fun _ -> "");
      print_open_tag = (fun tag -> if tag == magic_tag then (magic_cookie := Empty));
      print_close_tag = (fun _ -> ());
    };
  pp
end
