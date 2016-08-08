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
open WatchCommon
open WatchTypes
open WatchReportCommon
open WatchReportAnalyze

let report_file fi =
  assert (not fi.file_reported); fi.file_reported <- true

type file_operations = {
  mutable fo_pure_inputs : string list;
  mutable fo_modified_inputs : string list;
  mutable fo_temporaries : string list;
  mutable fo_pure_outputs : string list;
  mutable fo_pure_removes : string list;
  mutable fo_not_reported : string list;
}

let report file_ops fi list fmt =
  Printf.kprintf (fun s ->
    report_file fi;
    (Printf.sprintf "%s [%s]" s file_ops) :: list
  ) fmt

let print_list indent p list fmt =
  Printf.kprintf (fun s ->
    let len = List.length list in
    if len > 0 then begin
      Printf.printf "%s%d: FILES as %s\n" indent p.proc_pid s;
      List.iter (fun s -> Printf.printf "%s   %s\n" indent s)
        (List.sort compare list)
    end
  ) fmt



let rec print_process level indent p =
  begin match p.proc_father with
      None ->
        Printf.printf "%s_: fork %d\n" indent p.proc_pid;
    | Some { proc_pid = ppid } ->
      Printf.printf "%s%d: fork %d\n" indent ppid p.proc_pid;
  end;
  let indent = indent ^ "   " in
  List.iter (function
    | ProcActionExec(cmd, prog, prog_args, _) ->
      Printf.printf "%s%d: %s\t%s:\t%s\n"
        indent p.proc_pid cmd prog
        (String.concat " "
           (List.map (fun s -> Printf.sprintf "%S" s) prog_args))
    | ProcActionFork newp ->
      if level > 0 then
        print_process (level-1) indent newp
    | ProcActionCwd cwd ->
      if !print_open then
        Printf.printf "%s%d: chdir %S\n" indent p.proc_pid cwd
    | ProcActionOpen (filename, rights) ->
      if !print_open then
        Printf.printf "%s%d: open %S (%s)\n" indent p.proc_pid filename
          (match rights with
              OpenRead -> "ro"
            | OpenReadWrite -> "rw"
            | OpenWrite -> "wo"
            | Remove -> "rm"
          )
  ) p.proc_actions;

  let fo = {
    fo_pure_inputs = [];
    fo_modified_inputs = [];
    fo_temporaries = [];
    fo_pure_outputs = [];
    fo_pure_removes = [];
    fo_not_reported = [];
  } in

  if !print_files then begin
    StringMap.iter (fun _ fi ->

      let file_ops =
        (
          match fi.file_read with
              None -> []
            | Some (first_read, last_read) ->
              [ first_read, 'R'; last_read, 'R' ]
        ) @
          (
            match fi.file_write with
                None -> []
              | Some (first_write, last_write) ->
                [ first_write, 'W'; last_write, 'W' ]

          ) @
          (
            match fi.file_remove with
                None -> []
              | Some (first_remove, last_remove) ->
                [ first_remove, 'D'; last_remove, 'D' ]
          )
      in
      let file_ops = List.sort compare file_ops in

      let rec iter file_ops =
        match file_ops with
            ((time1, op1) as op) ::
              (
                ((time2, op2) :: tail2) as tail1 ) when
                  time1 = time2 ->
                    if op1 = op2 ||  (op2 = 'W' && op1 = 'D') then
                      iter (op :: tail2)
                    else
                      op :: iter tail1

          | op :: tail ->
            op :: iter tail
          | [] -> []
      in
      let file_ops = iter file_ops in

      let rec iter file_ops =
        match file_ops with
            ((time1, op1) as _op) ::
              (
                ((time2, op2) :: tail2) as tail1 ) when
                  op1 = op2 ->
                    iter tail1
          | op :: tail ->
            op :: iter tail
          | [] -> []
      in
      let file_ops = iter file_ops in


      let file_ops = List.map snd file_ops in
      let file_ops = Array.of_list file_ops in
      let file_ops =
        let s = Bytes.create (Array.length file_ops) in
        for i = 0 to Array.length file_ops - 1 do
          s.[i] <- file_ops.(i)
        done;
        Bytes.to_string s
      in

      match fi.file_read, fi.file_write, fi.file_remove with

        | Some _, None, _ ->
          fo.fo_pure_inputs <- report file_ops  fi fo.fo_pure_inputs
            "%s" fi.file_name;

        | Some (first_read, last_read), Some (first_write, last_write), _
          when first_read < first_write ->
          fo.fo_modified_inputs <- report file_ops  fi fo.fo_modified_inputs
            "%s%s" fi.file_name
            (match fi.file_remove with
                None -> ""
              | Some (first_remove, last_remove) ->
                if last_remove >= last_write then " (removed)"
                else "");

        | None, Some (_, last_write), Some (_, last_remove)
          when last_write < last_remove ->
          fo.fo_temporaries <- report file_ops  fi fo.fo_temporaries "%s" fi.file_name

        | Some (first_read, _),
            Some (first_write, last_write), Some (_, last_remove) when
                last_write <= last_remove &&
                  first_read > first_write ->
          fo.fo_temporaries <- report file_ops  fi fo.fo_temporaries "%s" fi.file_name

        | None, Some _, None ->
          fo.fo_pure_outputs <- report file_ops  fi fo.fo_pure_outputs
            "%s" fi.file_name;

        | None, Some wo_seq, Some rm_seq when
            wo_seq = rm_seq ->
          fo.fo_pure_removes <- report file_ops  fi fo.fo_pure_removes "%s" fi.file_name;
        | _ ->
          fo.fo_not_reported <- report file_ops  fi fo.fo_not_reported "%s%s%s%s"
            fi.file_name

            (match fi.file_read with
                None -> ""
              | Some (first, last) ->
                if first = last then
                  Printf.sprintf "(rd %d)" first
                else
                  Printf.sprintf "(rd %d-%d)" first last)

            (match fi.file_write with
                None -> ""
              | Some (first, last) ->
                if first = last then
                  Printf.sprintf "(wr %d)" first
                else
                  Printf.sprintf "(wr %d-%d)" first last)
            (match fi.file_remove with
                None -> ""
              | Some (first, last) ->
                if first = last then
                  Printf.sprintf "(rm %d)" first
                else
                  Printf.sprintf "(rm %d-%d)" first last)

    ) p.proc_files_rec;


    print_list indent p fo.fo_pure_inputs "PURE INPUTS (read only)";
    print_list indent p fo.fo_modified_inputs "MODIFIED INPUTS (read, then write)";
    print_list indent p fo.fo_temporaries "TEMPORARIES (create, then remove)";
    print_list indent p fo.fo_pure_outputs "PURE OUTPUTS (write only)";
    print_list indent p fo.fo_pure_removes "PURE REMOVE (remove only)";
    print_list indent p fo.fo_not_reported "NOT REPORTED";

  end

let print_event ev_seq ev =
  if !print_calls && ( !all_pids || IntSet.mem ev.ev_pid !pids ) then
    Printf.printf "%d.%03d:pid %d : %s\n%!"
      (ev.ev_time / 1000) (ev.ev_time mod 1000)
      ev.ev_pid (string_of_message ev.ev_msg)

let subcmd_init () = ()
let subcmd_spec = {
  subcmd_list = WatchReportCommon.arg_list;
  subcmd_usage = arg_usage;
  subcmd_help = [];
}

let subcmd_main args =

  let file = match args with
      [| file |] -> file
    | [||] ->
      Printf.eprintf "Error: you must provide the name of the data file\n%!";
      raise Subcommands.Usage
    | _ ->
      Printf.eprintf "Error: you must provide only the name of the data file\n%!";
      raise Subcommands.Usage
  in

  if !print_open || !print_files then print_pstree := true;

  if not !print_calls && not !print_pstree then
    print_calls := true;

  WatchReportAnalyze.analyze print_event file;


  if !warning_could_not_follow_opens then begin
    Printf.eprintf "Warning: could not follow all open functions\n%!";
  end;

  if !print_pstree then
    Hashtbl.iter (fun _ p ->
      if !all_pids then begin
        match p.proc_father with
            Some _ -> ()
          | None ->
            if p.proc_pid <> 0 then
              print_process !level "" p
      end else
        if IntSet.mem p.proc_pid !pids then
          print_process !level "" p
    ) processes
