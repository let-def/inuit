#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "inuit" @@ fun c ->
  Ok [ Pkg.mllib "src/inuit.mllib" ]
