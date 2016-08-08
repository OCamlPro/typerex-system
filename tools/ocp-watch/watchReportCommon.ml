(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open WatchFunctions
open WatchCommon
open WatchTypes

let warning_could_not_follow_opens = ref false
let all_pids = ref true
let pids = ref IntSet.empty
let level = ref max_int
let verbose = ref false
let print_open = ref false
let print_files = ref false
let just_top = ref false
let print_calls = ref false
let print_pstree = ref false

let arg_list = [
  "-pstree", Arg.Set print_pstree, " : print a process tree";
  "-calls", Arg.Set print_calls, " : print all function calls";
  "-top", Arg.Unit (fun _ ->
    just_top := true;
    level := 0), " : print only top process info";
  "-pid", Arg.Int (fun pid ->
    all_pids := false;
    pids := IntSet.add pid !pids), " PID : print only info for PID";
  "-only", Arg.Int (fun pid ->
    all_pids := false;
    pids := IntSet.add pid !pids;
    level := 0
  ), " PID : print only info for PID";
  "-r", Arg.Int (fun n -> level := n), " LEVEL : print only this level";
  "-v", Arg.Set verbose, " : set verbose";

  "-all", Arg.Unit (fun _ ->
    print_open := true;
    print_files := true;
    print_calls := true;
    print_pstree := true;
  ), " : print all infos";
  "-open", Arg.Set print_open, " : print opens";
  "-files", Arg.Set print_files, " : print files";
]

let arg_usage =
  [
    "ocp-watch report [OPTIONS] data_filename";
    "";
    "print information on all processes involved in the data filename";
  ]

type processus = {
  proc_pid : int;
  mutable proc_actions : process_action list;
  mutable proc_father : processus option;
  mutable proc_events : p2w_msg list;
  mutable proc_cwd : string option;
  mutable proc_files : file_info StringMap.t;
  mutable proc_files_rec : file_info StringMap.t;
  mutable proc_fds : file_info IntMap.t;
}

and file_info = {
  file_name : string;
  mutable file_read : (int * int) option;
  mutable file_write : (int * int) option;
  mutable file_remove : (int * int) option;
  mutable file_reported : bool;
}

and open_flags = OpenRead | OpenWrite | OpenReadWrite | Remove

and process_action =
    ProcActionExec of (string * string * string list * string list)
  | ProcActionFork of processus
  | ProcActionCwd of string
  | ProcActionOpen of string * open_flags


let (processes  : (int, processus) Hashtbl.t) =
  Hashtbl.create 113
