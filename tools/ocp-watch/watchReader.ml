(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open StringCompat
open WatchFunctions
open WatchTypes

let read16 s pos =
  let cmd0 = int_of_char s.[pos] in
  let cmd1 = int_of_char s.[pos+1] in
  let cmd = cmd0 + (cmd1 lsl 8) in
  cmd, pos+2



let rec read_long s pos =
  let c = s.[pos] in
  let pos = pos + 1 in
  let n = int_of_char c in
  let nn = n land 0x7f in
  if n = nn then
    let (x, pos) = read_long s pos in
    (n + (x lsl 7), pos)
  else
    (nn, pos)

let rec read_arg s pos =
  let c = int_of_char s.[pos] in
  let pos = pos + 1 in
  match c with
    | 0 -> Arg_string None, pos
    | 1 ->
      let (len, pos) = read16 s pos in
      let ss = String.sub s pos len in
      let pos = pos + len in
      Arg_string (Some ss), pos
    | 2 -> Arg_strings None, pos
    | 3 ->
      let rec iter pos list =
        let (arg, pos) = read_arg s pos in
        match arg with
            Arg_string None ->
              (List.rev list, pos)
          | Arg_string (Some s) ->
            iter pos (s :: list)
          | _ -> assert false
      in
      let (list, pos) = iter pos [] in
      Arg_strings (Some list), pos
    | 4 ->
      let (x, pos) = read_long s pos in
      Arg_long x, pos
    | 5 ->
      let (x, pos) = read_long s pos in
      Arg_long (-x), pos
    | 6 ->
      let (x, pos) = read_long s pos in
      Arg_ulong x, pos
    | 7 ->
      Arg_FILE None, pos
    | 8 ->
      let (x, pos) = read_long s pos in
      Arg_FILE (Some x), pos
    | 9 ->
      Arg_DIR None, pos
    | 10 ->
      let (x, pos) = read_long s pos in
      Arg_DIR (Some x), pos
    | 11 ->
      Arg_STAT None, pos
    | 12 ->
      let (st_dev, pos) = read_long s pos in
      let (st_ino, pos) = read_long s pos in
      let (st_mode, pos) = read_long s pos in
      let (st_size, pos) = read_long s pos in
      let (st_uid, pos) = read_long s pos in
      let (st_gid, pos) = read_long s pos in
      let (st_mtime, pos) = read_long s pos in
      Arg_STAT (Some {
        st_dev;   st_ino;  st_mode;
        st_size;  st_uid;  st_gid;
        st_mtime;
      }), pos
    | 13 ->
      Arg_DIRENT None, pos
    | 14 ->
      let (d_ino, pos) = read_long s pos in
      let (arg, pos) = read_arg s pos in
      let d_name =
        match arg with
            Arg_string (Some s) -> s
          | _ -> assert false
      in
      Arg_DIRENT (Some { d_ino; d_name} ), pos
    | 15 ->
      let (x, pos) = read_long s pos in
      Arg_errno x, pos
    | _ ->
      Printf.eprintf "Error: unknown arg code %d\n" c;
      exit 2

let rec read_args s pos len =
  if pos = len then [] else
    if pos > len then assert false else
      let (arg, pos) = read_arg s pos in
      arg :: read_args s pos len


let buf = Bytes.create 8000
let input_event ic =
  let ntoread = input_binary_int ic in
  let ev_time = input_binary_int ic in
  really_input ic buf 0 ntoread;
  let s = Bytes.sub_string buf 0 ntoread in
  let cmd, pos = read16 s 0 in
  let ev_pid, pos = read16 s pos in
  let ev_msg =
    match cmd with
    | 4 -> (* ProcInfoEvent *)
      let cmd_len, pos = read16 s pos in
      let env_len, pos = read16 s pos in
      let proc_cmd = String.sub s pos cmd_len in
      let pos = pos + cmd_len in
      let proc_env = String.sub s pos env_len in
      ProcInfoEvent {
        proc_cmd = proc_cmd;
        proc_env = proc_env;
      }

    | _ ->
      let fun_id, pos = read16 s pos in
      let arg_len, pos = read16 s pos in
      let args = read_args s 8 (8 + arg_len) in
      match cmd with
      | 0 -> KillMsg
      | 1 -> ProcMsg ( (* ppid *) fun_id, (* cwd *) args)
      | 2 -> BeforeCallMsg (fun_id, args)
      | 3 -> AfterCallMsg (fun_id, args)
      | _ ->
        Printf.eprintf "Error: unknown command %d\n%!" cmd;
        exit 2
  in
  { ev_pid ; ev_time; ev_msg }

let iter_events f g filename =
  if not (Sys.file_exists  filename) then begin
    Printf.eprintf "Error: file %S does not exist.\n%!" filename;
    exit 2
  end;
  let ic = open_in filename in
  let pre = (input_value ic : preamble) in
  assert (pre.pre_version = 1);
  f pre;
  try
    let counter = ref 0 in
    while true do
      let ev = input_event ic in
      g !counter ev;
      incr counter
    done;
    assert false
  with End_of_file ->
    close_in ic
