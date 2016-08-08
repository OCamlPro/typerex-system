(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open StringCompat

val output_event : out_channel -> WatchTypes.event -> unit
val str_int16 : bytes -> int -> int -> unit
val output_preamble : out_channel -> WatchTypes.preamble -> unit

val output_raw_event : out_channel ->
 (* time *) int ->
 (* block *) string -> (* len *) int -> unit
