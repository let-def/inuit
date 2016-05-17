include Inuit_base

type 'flags cursor = {
  region : 'flags region;
  flags : 'flags list;
}

module Cursor =
struct
  type 'flags clickable = [> `Clickable | `Clicked] as 'flags

  let null = { region = Region.null; flags = [] }

  let text t ?(flags=t.flags) text =
    Region.append t.region flags text

  let clear t =
    Region.clear t.region

  let sub t = { region = Region.sub t.region; flags = t.flags }

  let observe { region; flags } f =
    let observer region =
      let t' = { region; flags } in
      fun side patch -> f t' side patch
    in
    { region = Region.sub ~observer region; flags }

  let is_closed t = Region.is_closed t.region

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
    let t =
      if List.mem `Clickable t.flags then t
      else {t with flags = `Clickable :: t.flags}
    in
    observe t (
      fun t' side patch ->
        if List.mem `Clicked patch.Patch.flags then
          Some (fun () -> f t')
        else None
    )

  let printf t ?flags fmt =
    Printf.ksprintf (text t ?flags) fmt

  let link t ?flags fmt f =
    let t' = clickable t f in
    Printf.ksprintf (text t' ?flags) fmt

  let cursor_of_region ?(flags=[]) region =
    { region; flags = flags }

  let make () =
    let region, pipe = Region.create () in
    cursor_of_region region, pipe
end
