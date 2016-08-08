(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open StringCompat

open Subcommands.TYPES
open WatchFunctions
open WatchTypes
open WatchCommon
open Msgipc.TYPES

type mq_pipe = {
  mq_P2W : Msgipc.t;  (* process 2 watcher *)
  mq_W2P : Msgipc.t;  (* watcher 2 process *)
}

let mq_pipe_create () =
  let mq_P2W = Msgipc.create_private () in
  let mq_W2P = Msgipc.create_private () in
  { mq_P2W = mq_P2W; mq_W2P = mq_W2P }

let verbosity = ref 0

let protocol_W2P_BitmapMsg = 0
let protocol_W2P_ContinueMsg = 1

let default_kill_file = "ocp-watch.kill"
let load_default_kill_file = ref true
let option_flags = ref 0
let target_file = ref "ocp-watch.bin"

let function_bitmap = Bytes.make (22+nfunctions) '\003'

let is_sync_function fun_id =
  (int_of_char
     (Bytes.get function_bitmap (22+fun_id))) land 4 <> 0

let set_function_action fun_name action_code =
  let fun_id = function_id fun_name in
  function_bitmap.[22+fun_id] <- char_of_int action_code

let function_bitmap_msg pid flags =
  WatchWriter.str_int16 function_bitmap 0 protocol_W2P_BitmapMsg;
  WatchWriter.str_int16 function_bitmap 2 flags;
  WatchWriter.str_int16 function_bitmap 4 nfunctions;
  String.blit digest 0 function_bitmap 6 16;
  {
    msg_id = pid;
    msg_size = Bytes.length function_bitmap;
    msg_payload = Bytes.to_string function_bitmap;
  }

let continue_msg pid fun_id =
  let msg = Bytes.create 4 in
  WatchWriter.str_int16 function_bitmap 0 protocol_W2P_ContinueMsg;
  WatchWriter.str_int16 function_bitmap 2 fun_id;
  let msg = Bytes.to_string msg in
  {
    msg_id = pid;
    msg_size = String.length msg;
    msg_payload = msg;
  }

let kill_msg_len = 8
let kill_msg = {
  msg_id = 1; (* 1 is for the master, others should use the PID *)
  msg_size = kill_msg_len;
  msg_payload = String.make kill_msg_len '\000';
}

let fork_writer mq_pipe command_pid prog prog_args =
  if !verbosity > 0 then
    Printf.eprintf "fork_writer...\n%!";
  let pid = Unix.fork() in
  if pid > 0 then pid else
    if pid < 0 then begin
      Printf.eprintf "Error: unable to start writer subprocess. Exiting.\n%!";
      exit 2
    end else
      let bitmap_msg = function_bitmap_msg 0 !option_flags in
      let oc = open_out !target_file in
      let time0F = Unix.gettimeofday () in
      WatchWriter.output_preamble oc
        { pre_version = WatchTypes.format_version;
          pre_functions = Array.map (fun (_, fun_name, _, _) -> fun_name)
            WatchFunctions.functions;
          pre_time0 = time0F;
          pre_bitmap = bitmap_msg.msg_payload;
        };
      WatchWriter.output_event oc
        { ev_time = 0;
          ev_pid = command_pid;
          ev_msg = BeforeCallMsg(function_id "execvp", [
            Arg_string (Some prog);
            Arg_strings (Some (Array.to_list prog_args))])
        };
      let len_buf = 32000 in
      let s = Bytes.create len_buf in
      let msg = {
        msg_id = 0;
        msg_size = 0;
        msg_payload = s;
      } in
      let rec iter () =
        Msgipc.recv mq_pipe.mq_P2W 1 msg;
        assert (msg.msg_id = 1);
        let nread = msg.msg_size in
        let timeF = Unix.gettimeofday () in
        let msF = (timeF -. time0F) *. 1000. in
        let ms = int_of_float msF in
        let s = Bytes.to_string s in
        WatchWriter.output_raw_event oc ms s nread;
        let cmd, pos = WatchReader.read16 s 0 in
        let pid, pos = WatchReader.read16 s pos in
        begin
          match cmd with
            | 0 ->
              close_out oc;
              exit 0
            | 1 -> (* new process *)
              let ppid, _pos = WatchReader.read16 s pos in
              if !verbosity > 0 then
                Printf.eprintf "pid %d : created from %d\n%!" pid ppid;
              let proc_cmd = FileString.read_file
                (Printf.sprintf "/proc/%d/cmdline" pid) in
              let proc_env = FileString.read_file
                (Printf.sprintf "/proc/%d/environ" pid) in
              WatchWriter.output_event oc
                { ev_time = 0;
                  ev_pid = pid;
                  ev_msg = ProcInfoEvent {
                    proc_cmd = proc_cmd;
                    proc_env = proc_env;
                  }
                };

              Msgipc.send mq_pipe.mq_W2P { bitmap_msg with msg_id = pid };
              if !verbosity > 0 then
                Printf.eprintf "pid %d : created from %d (ack sent)\n%!" pid ppid;
            | 2 -> (* BeforeCallMsg *)
              let fun_id, pos = WatchReader.read16 s pos in
              if !verbosity > 1 then
                Printf.eprintf "pid %d : BeforeCallMsg %d\n%!" pid fun_id;
              let (_, _fun_name, _, _) = functions.(fun_id) in
              if is_sync_function fun_id then
                Msgipc.send mq_pipe.mq_W2P (continue_msg pid fun_id);
(*              Printf.eprintf "pid %d: fun %S\n%!" pid fun_name *)
              ()
            | 3 -> (* AfterCallMsg *)
              let fun_id, pos = WatchReader.read16 s pos in
              let (_, _fun_name, _, _) = functions.(fun_id) in
(*              Printf.eprintf "pid %d: res %S\n%!" pid fun_name *)
              ()
            | _ -> assert false
        end;
        iter ()
      in
      try
        iter ()
      with Unix.Unix_error _ as e ->
        Printf.eprintf "Exception %s in exec\n%!" (Printexc.to_string e);
      exit 2


let fork_command mq_pipe prog prog_args =
  let pid = Unix.fork() in
    if pid > 0 then pid else
    if pid < 0 then begin
      Printf.eprintf "Error: unable to start writer subprocess. Exiting.\n%!";
      exit 2
    end else begin
      Unix.putenv "LD_PRELOAD" ocp_watcher_so;
      let mq_P2W_id = Msgipc.to_int mq_pipe.mq_P2W in
      let mq_W2P_id = Msgipc.to_int mq_pipe.mq_W2P in
      if !verbosity > 0 then begin
        Printf.eprintf "OCP_WATCH_MQ_P2W = %d\n%!" mq_P2W_id;
        Printf.eprintf "OCP_WATCH_MQ_W2P = %d\n%!" mq_W2P_id;
      end;
      Unix.putenv "OCP_WATCH_MQ_P2W"  (string_of_int mq_P2W_id);
      Unix.putenv "OCP_WATCH_MQ_W2P"  (string_of_int mq_W2P_id);
      (*      Unix.putenv "OCP_WATCH_DATA" data_filename; *)
      try
        Unix.execvp prog prog_args
      with Unix.Unix_error _ as e ->
        Printf.eprintf "Exception %s in exec\n%!" (Printexc.to_string e);
        exit 2
    end



let load_kill_file filename =
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      set_function_action line 0;
    done; assert false
  with End_of_file ->
  close_in ic

let arg_list = [
  "-flags", Arg.Int (fun i -> option_flags := i), " FLAGS : set option flags";
  "-o" , Arg.String (fun s -> target_file := s), " FILE : set target data file";
  "-kill-file", Arg.String load_kill_file, " FILE : file containing functions to disregard";
  "-no-kill-file", Arg.Clear load_default_kill_file,
  " : do not load default kill file (ocp-watch.kill)";
  "-v", Arg.Unit (fun _ -> incr verbosity), " : incr verbosity";
]

let arg_usage =
  [
    "[OPTIONS] command [args]";
    "";
    "record functions called in libc by command with args (in ocp-watch.bin)";
  ]

let subcmd_init () =
  if not (Sys.file_exists ocp_watcher_so) then begin
    Printf.eprintf "Error: you must use 'ocp-watch install' first to generate %s\n%!" ocp_watcher_so;
    exit 2
  end;

  Array.iteri
    (fun i (_, fun_name, _, _) ->
      Hashtbl.add hfun fun_name i) functions

let subcmd_spec = {
  subcmd_list = arg_list;
  subcmd_usage = arg_usage;
  subcmd_help = [];
}

let subcmd_main prog_args =
  if Array.length prog_args = 0 then begin
    Printf.eprintf "Error: you must at least provide the command to call\n%!";
    raise Subcommands.Usage
  end;
  let prog = prog_args.(0) in

  if !load_default_kill_file && Sys.file_exists default_kill_file then
    load_kill_file default_kill_file;

  if Sys.file_exists !target_file then
    Sys.remove !target_file;
  Printf.eprintf "Events will be saved in %s\n%!" !target_file;
  let mq = mq_pipe_create () in
  let command_pid = fork_command mq prog prog_args in
  let writer_pid = fork_writer mq command_pid prog prog_args in
  let writer_dead = ref false in
  let command_dead = ref false in
  let command_status = ref "" in
  while not !writer_dead || not !command_dead do
    try
      let (pid, status) = Unix.wait() in
      if pid = command_pid then begin
        begin
          match status with
            Unix.WEXITED n ->
              command_status := Printf.sprintf "exit %d" n
          | Unix.WSIGNALED n  ->
            command_status := Printf.sprintf "signal %d" n
          | Unix.WSTOPPED _ -> assert false;
        end;
        command_dead := true;
        if not !writer_dead then
          Msgipc.send mq.mq_P2W kill_msg
      end
      else
        if pid = writer_pid then begin
          writer_dead := true;
          if !command_dead then begin
            Printf.fprintf stderr "Process exited (%s)\n%!" !command_status;
            exit 0;
          end;
          Printf.eprintf "Warning: subprocess writer dead before command. Losing events !\n";
        end
    with Unix.Unix_error _ as e ->
      Printf.eprintf "Exception %s in wait\n%!" (Printexc.to_string e);
      exit 2
  done
