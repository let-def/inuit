include Inuit_base

type 'flags cursor = {
  region : 'flags region;
  style : 'flags list;
}

let text t ?(flags=t.style) text =
  Region.append t.region flags text

let clear t =
  Region.clear t.region

let sub t = { region = Region.sub t.region; style = t.style }

let observe { region; style } f =
  let observer region =
    let t' = { region; style } in
    fun side patch -> f t' side patch
  in
  { region = Region.sub ~observer region; style }

let is_closed t = Region.is_closed t.region

let default t = t.style
let with_default style t = { t with style }
let region t = t.region

let action t f =
  let t =
    if List.mem `Clickable t.style then t
    else {t with style = `Clickable :: t.style}
  in
  observe t (
    fun t' side patch ->
      if List.mem `Clicked patch.Patch.flags then
        Some (fun () -> f t')
      else None
  )

let printf t ?flags fmt =
  Printf.ksprintf (text t ?flags) fmt

let link t ?flags f fmt =
  let t' = action t f in
  Printf.ksprintf (text t' ?flags) fmt
