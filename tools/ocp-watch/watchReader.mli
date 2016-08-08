(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

val read16 : string -> int -> int * int
val input_event : in_channel -> WatchTypes.event


val iter_events :
  (WatchTypes.preamble -> unit) ->
  (int -> WatchTypes.event -> unit) -> string -> unit
