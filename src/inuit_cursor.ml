open Inuit_base

type 'flags cursor = {
  region : 'flags Inuit_region.t;
  flags  : 'flags list;
  indent : int;
}

type 'flags clickable = [> `Clickable | `Clicked] as 'flags

let null = { region = Inuit_region.null; flags = []; indent = 0 }

let count_char str chr =
  let count = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = chr then incr count;
  done;
  !count

let indent_text col text =
  if col <= 0 then text else
    let count = count_char text '\n' in
    if count = 0 then text else
      let buf = Bytes.make (String.length text + col * count) ' ' in
      let rec fill src dst =
        match String.index_from text src '\n' with
        | exception Not_found ->
          Bytes.blit_string text src buf dst (String.length text - src)
        | src' ->
          let len = src' - src + 1 in
          Bytes.blit_string text src buf dst len;
          fill (src' + 1) (dst + len + col)
      in
      fill 0 0;
      Bytes.unsafe_to_string buf

let text t ?(flags=t.flags) text =
  Inuit_region.append t.region flags (indent_text t.indent text)

let clear t =
  Inuit_region.clear t.region

let kill t =
  Inuit_region.kill t.region

let sub t = { t with region = Inuit_region.sub t.region }

let observe { region; flags; indent } f =
  let observer region =
    let t' = { region; flags; indent } in
    fun side patch -> f t' side patch
  in
  { region = Inuit_region.sub ~observer region; flags; indent }

let is_closed t = Inuit_region.is_closed t.region

let mem_flag flag cursor =
  List.mem flag cursor.flags

let add_flag flag cursor =
  if mem_flag flag cursor
  then cursor
  else {cursor with flags = flag :: cursor.flags}

let rem_flag flag cursor =
  if mem_flag flag cursor
  then {cursor with flags = List.filter ((<>) flag) cursor.flags}
  else cursor

let get_flags t = t.flags

let with_flags flags t = { t with flags }

let region t = t.region

let clickable t f =
  let t = add_flag `Clickable t in
  observe t (
    fun t' side patch ->
      let {Patch. flags; offset} =  patch in
      if Inuit_region.unsafe_right_offset t'.region > offset &&
         List.mem `Clicked flags then
        (List.filter ((<>) `Clicked) flags, Some (fun () -> f t'))
      else
        (flags, None)
  )

let printf t ?flags fmt =
  Printf.ksprintf (text t ?flags) fmt

let link t ?flags fmt =
  Printf.ksprintf (fun str f -> text (clickable t f) ?flags str) fmt

let cursor_of_region ?(flags=[]) ?(indent=0) region =
  { region; flags = flags; indent }

let make () =
  let region, pipe = Inuit_region.make () in
  cursor_of_region region, pipe

let get_indent t = t.indent

let with_indent t indent = {t with indent}

let shift_indent t indent = {t with indent = max 0 (t.indent + indent) }
