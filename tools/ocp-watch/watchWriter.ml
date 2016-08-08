(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open StringCompat
open WatchTypes

let buf_int8 b n = Buffer.add_char b (char_of_int n)

let buf_int16 b n =
  buf_int8 b (n land 0xff);
  buf_int8 b ( (n lsr 8) land 0xff);
  ()

let str_int8 s pos n = s.[pos] <- char_of_int n

let str_int16 s pos n =
  str_int8 s pos (n land 0xff);
  str_int8 s (pos+1) ( (n lsr 8) land 0xff);
  ()

let rec buf_arg b arg =
  match arg with
      Arg_string None ->
        buf_int8 b 0
    | Arg_string (Some s) ->
      buf_int8 b 1;
      buf_int16 b (String.length s);
      Buffer.add_string b s
    | Arg_strings None ->
      buf_int8 b 2
    | Arg_strings (Some list) ->
      buf_int8 b 3;
      List.iter (fun s -> buf_arg b (Arg_string (Some s))) list;
      buf_arg b (Arg_string None)
    | _ -> assert false

let write_args args =
  let b = Buffer.create 8000 in
  List.iter (buf_arg b) args;
  Buffer.contents b

let output_raw_event oc ev_time s len =
  output_binary_int oc len;
  output_binary_int oc ev_time;
  output_substring oc s 0 len

let output_event oc ev =
  let b = Buffer.create 8000 in
  begin
    match ev.ev_msg with
      | BeforeCallMsg (fun_id, args) ->
        buf_int16 b id_BeforeCallMsg;
        buf_int16 b ev.ev_pid;
        buf_int16 b fun_id;
        let s = write_args args in
        buf_int16 b (String.length s);
        Buffer.add_string b s
      | KillMsg -> assert false
      | ProcMsg _ -> assert false
      | AfterCallMsg _ -> assert false
      | ProcInfoEvent proc ->
        buf_int16 b id_ProcInfoEvent;
        buf_int16 b ev.ev_pid;
        buf_int16 b (String.length proc.proc_cmd);
        buf_int16 b (String.length proc.proc_env);
        Buffer.add_string b proc.proc_cmd;
        Buffer.add_string b proc.proc_env;
  end;
  let s = Buffer.contents b in
  let len = String.length s in
  output_raw_event oc ev.ev_time s len

let output_preamble oc pre =
  output_value oc pre
