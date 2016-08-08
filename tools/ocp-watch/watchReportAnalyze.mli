(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

val analyze : (int -> WatchTypes.event -> unit) -> string -> unit
