include Inuit_base

type 'flags cursor = {
  region : 'flags region;
  flags : 'flags list;
}

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

let get_flags t = t.flags
let with_flags flags t = { t with flags }
let region t = t.region

let action t f =
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
  let t' = action t f in
  Printf.ksprintf (text t' ?flags) fmt

let cursor_of_region ?(flags=[]) region =
  { region; flags = flags }

let make () =
  let region = Region.create () in
  cursor_of_region region, Region.pipe region
