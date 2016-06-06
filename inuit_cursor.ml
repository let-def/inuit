open Inuit_base

type 'flags cursor = {
  region : 'flags Inuit_region.t;
  flags : 'flags list;
}

type 'flags clickable = [> `Clickable | `Clicked] as 'flags

let null = { region = Inuit_region.null; flags = [] }

let text t ?(flags=t.flags) text =
  Inuit_region.append t.region flags text

let clear t =
  Inuit_region.clear t.region

let kill t =
  Inuit_region.kill t.region

let sub t = { region = Inuit_region.sub t.region; flags = t.flags }

let observe { region; flags } f =
  let observer region =
    let t' = { region; flags } in
    fun side patch -> f t' side patch
  in
  { region = Inuit_region.sub ~observer region; flags }

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
      if List.mem `Clicked patch.Patch.flags then
        Some (fun () -> f t')
      else None
  )

let printf t ?flags fmt =
  Printf.ksprintf (text t ?flags) fmt

let link t ?flags fmt =
  Printf.ksprintf (fun str f -> text (clickable t f) ?flags str) fmt

let cursor_of_region ?(flags=[]) region =
  { region; flags = flags }

let make () =
  let region, pipe = Inuit_region.create () in
  cursor_of_region region, pipe
