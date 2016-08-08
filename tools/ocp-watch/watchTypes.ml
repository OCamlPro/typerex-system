(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

let format_version = 1


type stat = {
  st_dev : int;
  st_ino : int;
  st_mode : int;
  st_size : int;
  st_uid : int;
  st_gid : int;
  st_mtime : int;
}

type dirent = {
  d_ino : int;
  d_name : string;
}

type arg =
  | Arg_string of string option
  | Arg_strings of string list option
  | Arg_long of int
  | Arg_ulong of int
  | Arg_FILE of int option
  | Arg_DIR of int option
  | Arg_STAT of stat option
  | Arg_DIRENT of dirent option
  | Arg_errno of int

type proc_info = {
  proc_cmd : string;
  proc_env : string;
}

type p2w_msg =
  | KillMsg
  | ProcMsg of int * arg list
  | BeforeCallMsg of int * arg list
  | AfterCallMsg of int * arg list
  | ProcInfoEvent of proc_info

let id_KillMsg = 0
let id_ProcMsg = 1
let id_BeforeCallMsg = 2
let id_AfterCallMsg = 3
let id_ProcInfoEvent = 4

type event = {
  ev_pid : int;
  ev_time : int;
  ev_msg : p2w_msg;
}

type preamble = {
  pre_version : int;
  pre_time0 : float;
  pre_functions : string array;
(*
   int16 : flags;
   int16 : nfunctions;
   int8[functions] : function_flags;
*)
  pre_bitmap : string;
}







open WatchFunctions

let string_of_arg = function
  | Arg_string None -> "STRING(NULL)"
  | Arg_string (Some s) ->
    Printf.sprintf "STRING(%S)" s
  | Arg_long n -> string_of_int n
  | Arg_ulong n -> string_of_int n
  | Arg_FILE None -> "FILE(NULL)"
  | Arg_FILE (Some n) -> Printf.sprintf "FILE(%d)" n
  | Arg_DIR None -> "DIR(NULL)"
  | Arg_DIR (Some n) -> Printf.sprintf "DIR(%d)" n
  | Arg_STAT None -> "STAT(NULL)"
  | Arg_STAT (Some st) ->
    Printf.sprintf "STAT{ st_size = %d; st_mode = %o }" st.st_size st.st_mode
  | Arg_DIRENT None -> "DIRENT(NULL)"
  | Arg_DIRENT (Some { d_ino; d_name }) ->
    Printf.sprintf "DIRENT{ d_ino=%d; d_name=%S }" d_ino d_name
  | Arg_strings None -> "STRINGS(NULL)"
  | Arg_strings (Some list) ->
    Printf.sprintf "STRINGS(%s)"
      (String.concat "," (List.map
                            (fun s -> Printf.sprintf "%S" s) list))
  | Arg_errno n ->
    Printf.sprintf "errno(%d)" n

let string_of_args args =
  String.concat "," (List.map string_of_arg args)

let string_of_message msg =
  match msg with
      KillMsg -> "KILL"
    | ProcMsg (ppid, args) ->
      Printf.sprintf "PROC of %d in %s" ppid
        (string_of_args args)
    | BeforeCallMsg (fun_id, args) ->
      Printf.sprintf "Before call %S (%s)"
        (function_name fun_id)
        (string_of_args args)
    | AfterCallMsg (fun_id, args) ->
      Printf.sprintf "After call %S (%s)"
        (function_name fun_id)
        (string_of_args args)
    | ProcInfoEvent proc ->
      Printf.sprintf "Proc info %S (env %S)" proc.proc_cmd proc.proc_env
