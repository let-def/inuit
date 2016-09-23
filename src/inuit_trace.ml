external reraise : exn -> 'a = "%reraise"

type 'flags trace = {
  cursor: 'flags Inuit_cursor.cursor;
  depth: int;
}

let null = { cursor = Inuit_cursor.null; depth = 0 }

let initial ?(depth=3) k = { cursor = Inuit_cursor.sub k; depth }

let sub t =
  if Inuit_cursor.is_closed t.cursor then null
  else {
    cursor = Inuit_cursor.shift_indent (Inuit_cursor.sub t.cursor) (+1);
    depth = if t.depth = 0 then 3 else t.depth - 1;
  }

let button_text opened =
  if opened then "[-]" else "[+]"

(*let enter name printer_arg printer_res f t arg =*)

let display_result t time_ms msg =
  if time_ms > 0.1 then
    Inuit_cursor.printf t "%s (in %.02f ms)" msg time_ms
  else
    Inuit_cursor.text t msg

let call name printer_arg printer_res f t arg =
  if Inuit_cursor.is_closed t.cursor then f t arg else (
    Inuit_cursor.text t.cursor "\n";
    let button = Inuit_cursor.sub t.cursor in
    let opened = ref (t.depth > 0) in
    Inuit_cursor.printf t.cursor " %s(%s) = " name (printer_arg arg);
    let result = Inuit_cursor.sub t.cursor in
    Inuit_cursor.text result "...";
    let t' = sub t in
    let render () =
      Inuit_cursor.clear t'.cursor;
      let time = Sys.time () in
      match f (if !opened then t' else null) arg with
      | value ->
        let dtime = (Sys.time () -. time) *. 1000.0 in
        Inuit_cursor.clear result;
        display_result result dtime (printer_res value);
        value
      | exception exn ->
        let dtime = (Sys.time () -. time) *. 1000.0 in
        Inuit_cursor.clear result;
        display_result result dtime (Printexc.to_string exn);
        reraise exn
    in
    let _ =
      Inuit_cursor.link button "%s" (button_text !opened) @@ fun k ->
      opened := not !opened;
      Inuit_cursor.clear k;
      Inuit_cursor.text k (button_text !opened);
      ignore (render ())
    in
    render ()
  )

let fix name print_arg print_res f =
  let rec fix t =
    if Inuit_cursor.is_closed t.cursor then
      let rec fix' arg = f fix' arg in fix'
    else
      call name print_arg print_res (fun t -> f (fix t)) t
  in
  fix
