(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open WatchTypes
open WatchFunctions
open Msgipc.TYPES

let compare_int x y =
  if x = y then 0 else
    if x >= 0 then
      if y >= 0 then x - y else 1
    else
      if y >= 0 then -1 else x - y

module Int = struct type t = int let compare = compare_int end
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


let nfunctions = Array.length functions
let hfun = Hashtbl.create (2 * nfunctions)

let function_id fun_name =
  try
    Hashtbl.find hfun fun_name
  with Not_found ->
    Printf.eprintf "Error: unknown function %S\n%!" fun_name;
    exit 2


let register_function_names pre =
  Array.iteri (fun i fun_name ->
    Hashtbl.add hfun fun_name i
  ) pre.pre_functions

let args = Sys.argv
let nargs = Array.length args

let current_directory = Sys.getcwd ()

let data_filename = Filename.concat current_directory "ocp-watch.data"

let ocp_watcher_so = (Sys.getenv "HOME") ^ "/.ocp-watch/ocp_watcher.so"


let parse_args arg_list arg_usage =
  let args = Sys.argv in
  let nargs = Array.length args in
  let args = Array.sub args 1 (nargs-1) in
  args.(0) <- Printf.sprintf "ocp-watch %s" args.(0);
  let anon_args = ref [] in
  Arg.parse_argv args
    arg_list (fun s -> anon_args := s :: !anon_args) arg_usage;
  List.rev !anon_args

exception FoundCommand of int
let parse_args_before_command arg_list arg_usage =
  let args = Sys.argv in
  let nargs = Array.length args in
  let args = Array.sub args 1 (nargs-1) in
  args.(0) <- Printf.sprintf "ocp-watch %s" args.(0);
  try
    Arg.parse_argv args
      arg_list (fun s ->
        raise (FoundCommand !Arg.current)) arg_usage;
    [||]
  with FoundCommand current ->
    Array.sub args current (nargs-current-1)

