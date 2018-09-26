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

  type Format.stag +=
    | Cursor of (cursor -> cursor)
    | Flags of (M.flag list -> M.flag list)

  let formatter_of_cursor c =
    let stack = ref [Inuit_cursor.sub c; c] in
    let pp = Format.make_formatter
        (fun txt ofs len ->
           let txt =
             if ofs = 0 && len = String.length txt
             then txt
             else String.sub txt ofs len
           in
           match !stack with
           | c :: _ when Inuit_cursor.is_open c ->
             Inuit.Cursor.text c txt
           | _ :: ((c' :: _) as tail) when Inuit_cursor.is_open c' ->
             let c = Inuit_cursor.sub c' in
             stack := c :: tail;
             Inuit.Cursor.text c txt
           | _ -> ()
        )
        (fun () -> ())
    in
    let noop _ = "" in
    Format.pp_set_formatter_stag_functions pp {
      Format.
      mark_open_stag = (fun stag ->
        begin match stag, !stack with
          | Cursor f, ((x :: _) as tail) ->
            let x' = f x in
            stack := Inuit_cursor.sub x' :: tail
          | Flags f, ((x :: _) as tail) ->
            let flags = Inuit_cursor.get_flags x in
            let x' = Inuit_cursor.with_flags flags x in
            stack := Inuit_cursor.sub x' :: tail
          | _ -> ()
        end;
        ""
      );
      mark_close_stag = (fun stag ->
        begin match stag, !stack with
          | (Cursor _ | Flags _), (_ :: tail) ->
            stack := tail
          | _ -> ()
        end;
        ""
      );
      print_open_stag = ignore;
      print_close_stag = ignore;
    };
    Format.pp_set_tags pp true;
    pp

  let push_cursor f pp =
    Format.pp_open_stag pp (Cursor f)

  let push_flags f pp =
    Format.pp_open_stag pp (Flags f)

  let pop pp =
    Format.pp_close_stag pp ()

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
    Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())
end
