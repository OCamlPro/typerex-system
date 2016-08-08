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
open WatchReportCommon

(*

What this analysis does not see:
- direct syscalls
- passing file-descriptors to children
*)


(*
let stdin = { file_name = "<stdin>"; file_read = None; file_write = None }
let stdout = { file_name = "<stdout>"; file_read = None; file_write = None }
let stderr = { file_name = "<stderr>"; file_read = None; file_write = None }
*)

let find_pid pid =
  try
    Hashtbl.find processes pid
  with Not_found ->
    let p = {
      proc_pid = pid;
      proc_actions = [];
      proc_events = [];
      proc_father = None;
      proc_cwd = None;
      proc_files = StringMap.empty;
      proc_files_rec = StringMap.empty;
      proc_fds = IntMap.empty;
    } in
(*
    p.proc_fds <- IntMap.add 0 { stdin with file_read = true } p.proc_fds;
    p.proc_fds <- IntMap.add 1 { stdout with file_write = true } p.proc_fds;
    p.proc_fds <- IntMap.add 2 { stderr with file_write = true } p.proc_fds;
*)
    Hashtbl.add processes pid p;
    p

let rights_of_frights rights =
  match rights.[0] with
    | 'r' -> OpenRead
    | 'w' -> OpenWrite
    | 'a' -> OpenReadWrite
    | _ ->
      Printf.eprintf "rights_of_frights: %S unknown\n%!" rights;
      exit 2

let rights_of_rights rights =
  match rights land 0x3 with
      0 -> OpenRead
    | 1 -> OpenWrite
    | 2 -> OpenReadWrite
    | 3 -> OpenReadWrite (* should not happen ? *)
    | _ -> assert false

let file_update_read fi seq =
  match fi.file_read with
      None -> fi.file_read <- Some (seq, seq)
    | Some (first_seq, last_seq) ->
      if first_seq > seq || last_seq < seq then
        let first_seq = if first_seq > seq then seq else first_seq in
        let last_seq = if last_seq < seq then seq else last_seq in
        fi.file_read <- Some (first_seq, last_seq)

let file_update_write fi seq =
  match fi.file_write with
      None -> fi.file_write <- Some (seq, seq)
    | Some (first_seq, last_seq) ->
      if first_seq > seq || last_seq < seq then
        let first_seq = if first_seq > seq then seq else first_seq in
        let last_seq = if last_seq < seq then seq else last_seq in
        fi.file_write <- Some (first_seq, last_seq)

let file_update_remove fi seq =
  match fi.file_remove with
      None -> fi.file_remove <- Some (seq, seq)
    | Some (first_seq, last_seq) ->
      if first_seq > seq || last_seq < seq then
        let first_seq = if first_seq > seq then seq else first_seq in
        let last_seq = if last_seq < seq then seq else last_seq in
        fi.file_remove <- Some (first_seq, last_seq)

let canonic_filenames = Hashtbl.create 133

let string_split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let canonic_filename filename = (* filename MUST BE absolute *)
  if filename.[0] <> '/' then begin
    Printf.eprintf "Error: filename %S is not absolute in canonic_filename\n%!"
      filename;
    exit 2
  end;

  let list = string_split filename '/' in
  let rec iter rev_prefix suffix =
    match suffix with
      | "" :: suffix -> iter rev_prefix suffix
      | "." :: suffix -> iter rev_prefix suffix
      | ".." :: suffix ->
        let rev_prefix = match rev_prefix with
            [] -> []
          | _ :: rev_prefix -> rev_prefix
        in
        iter rev_prefix suffix
      | new_prefix :: suffix ->
        iter (new_prefix :: rev_prefix) suffix
      | [] -> List.rev rev_prefix
  in
  let list = iter [] list in
  "/" ^ (String.concat "/" list)

let canonic_filename filename =
  try
    Hashtbl.find canonic_filenames filename
  with Not_found ->
    let new_filename = canonic_filename filename in
(*
    Printf.eprintf "CANONIC[%S]=\n" filename;
    Printf.eprintf "        %S\n" new_filename;
*)
    let new_filename = if new_filename <> filename then new_filename else
        filename in
    Hashtbl.add canonic_filenames filename new_filename;
    new_filename

let process_file p fd seq filename rights =
  p.proc_actions <- ProcActionOpen (filename, rights) :: p.proc_actions;
  let filename =
    if Filename.is_relative filename then
      match p.proc_cwd with
          None -> assert false
        | Some cwd -> Filename.concat cwd filename
    else filename
  in
  let filename = canonic_filename filename in
  let fi =
    try
      StringMap.find filename p.proc_files
    with Not_found ->
      let fi = {
        file_name = filename;
        file_read = None;
        file_write = None;
        file_remove = None;
        file_reported = false;
      } in
      p.proc_files <- StringMap.add filename fi p.proc_files;
      fi
  in
  if fd >= 0 then begin
(*    Printf.printf "         WATCHING FD%d \n" fd; *)
    p.proc_fds <- IntMap.add fd fi p.proc_fds;
  end;
  match rights with
    | OpenReadWrite ->
      file_update_read fi seq;
      file_update_write fi seq;
    | OpenRead ->
      file_update_read fi seq;
    | OpenWrite ->
      file_update_write fi seq
    | Remove ->
      file_update_write fi seq;
      file_update_remove fi seq

let process_dup p old_fd new_fd =
  try
    let fi = IntMap.find old_fd p.proc_fds in
    p.proc_fds <- IntMap.add new_fd fi p.proc_fds
  with Not_found ->
(*    Printf.eprintf "Warning: pid %d: process_dup  %d -> %d failed\n%!"
      p.proc_pid old_fd new_fd *)
    ()

let process_fopen p fd seq filename rights =
  process_file p fd seq filename (rights_of_frights rights)

let process_open p fd seq filename rights =
  process_file p fd seq filename (rights_of_rights rights)

let process_close p fd =
  p.proc_fds <- IntMap.remove fd p.proc_fds

let process_cwd p cwd =
  if p.proc_cwd <> Some cwd then begin
    p.proc_cwd <- Some cwd;
    p.proc_actions <- ProcActionCwd cwd :: p.proc_actions
  end

let filename_at p dirfd filename =
  if dirfd = -100 then (* AT_FDCWD *)
    filename (* keep it relative to local directory *)
  else
    try
      let fi = IntMap.find dirfd p.proc_fds in
      Filename.concat fi.file_name filename
    with Not_found ->
      warning_could_not_follow_opens := true;
      Printf.eprintf "Warning: pid %d, filename_at (%d, %S) failed\n%!"
        p.proc_pid dirfd filename;
      Printf.sprintf "/???/%s" filename

let analyze print_event file =

  WatchReader.iter_events
    register_function_names
    (fun ev_seq ev ->

      print_event ev_seq ev;

    let p = find_pid ev.ev_pid in

(*    p.proc_events <- msg :: p.proc_events; *)
    let prev_event =
      match p.proc_events with
          previous_msg :: _ -> [ previous_msg ]
        | [] -> []
    in
    p.proc_events <- ev.ev_msg :: prev_event;

    if !verbose then begin
      if !all_pids || IntSet.mem ev.ev_pid !pids then
        Printf.eprintf "Event (%d): %s\n%!" ev.ev_pid (string_of_message ev.ev_msg);
    end;
    match ev.ev_msg, prev_event with
        KillMsg, _ -> ()

      | ProcInfoEvent proc, [ ProcMsg (ppid, [ Arg_string (Some cwd)]) ] ->
        let pp = find_pid ppid in
        begin match p.proc_father with
          | None ->
(* we might not have seen the fork(), if the father used "system" *)
            p.proc_father <- Some pp;
            pp.proc_actions <- ProcActionFork p :: pp.proc_actions;
            begin
              match pp.proc_events with
                  BeforeCallMsg(fun_id, [ Arg_string (Some s) ]) :: _ ->
                    p.proc_actions <-
                      ProcActionExec("system",
                                     "/bin/sh",
                                     [ "/bin/sh"; "-c"; s ], []) ::
                      p.proc_actions
                | _ -> ()
            end
          | Some { proc_pid } when proc_pid = ppid -> ()
          | _ -> assert false
        end;
        process_cwd p cwd


      | ProcMsg (_, [ Arg_string (Some _)]), _ -> ()

      | ProcInfoEvent _, _ -> assert false
      | ProcMsg _, _ -> assert false

      | BeforeCallMsg(fun_id, args), _ ->
        let fun_name = function_name fun_id in
        begin
          match fun_name, args with
            | ("execv" | "execl") , [ Arg_string (Some cmd); Arg_strings (Some args) ]
            | ("execvp" | "execlp"), [ Arg_string (Some cmd); Arg_strings (Some args) ]
              ->
              p.proc_actions <-
                ProcActionExec (fun_name, cmd, args, []) :: p.proc_actions
            | ("execve" | "execle"), [ Arg_string (Some cmd);
                           Arg_strings (Some args);
(*                           Arg_strings (Some env); *)
                         ]
            | ("execvpe" | "execlpe"), [ Arg_string (Some cmd);
                           Arg_strings (Some args);
(*                           Arg_strings (Some env); *)
                         ] ->
              p.proc_actions <-
                ProcActionExec (fun_name, cmd, args, (* env *) []) :: p.proc_actions
            | _ -> ()
        end

      | AfterCallMsg (fun_id, args),
          (BeforeCallMsg (prev_fun_id, prev_args) :: _)
          ->
        assert (fun_id = prev_fun_id);
        let fun_name = function_name fun_id in
        begin
          match fun_name, args, prev_args with


            | ("fork" |  "vfork"),
          [ Arg_long newpid; Arg_errno errno ], _ ->
              if newpid > 0 then
                let newp = find_pid newpid in
                p.proc_actions <- ProcActionFork newp :: p.proc_actions;
                newp.proc_father <- Some p;
                begin
                  match newp.proc_cwd with
                      None -> newp.proc_cwd <- p.proc_cwd
                    | _ -> ()
                end
            | ("fork" |  "vfork"), _, _ -> assert false

            | "chdir", [ Arg_long res; Arg_errno _ ],
              [ Arg_string (Some dir)] ->
              let new_cwd =
                if Filename.is_relative dir then
                  match p.proc_cwd with
                      None -> assert false
                    | Some cwd -> Filename.concat cwd dir
                else dir
              in
              process_cwd p new_cwd

            | "fchdir",
              [ Arg_long _; Arg_string (Some cwd); Arg_errno _ ],
              [ Arg_long fd ] ->
              process_cwd p cwd

            | ("fopen" | "fopen64"),
              [ Arg_FILE (Some fd); Arg_errno _ ],
              [ Arg_string (Some filename); Arg_string (Some rights) ]
              ->
              process_fopen p fd ev_seq filename rights

            | "freopen",
              [ Arg_FILE (Some fd); Arg_errno _ ],
              [ Arg_string (Some filename); Arg_string (Some rights);
                Arg_FILE old_fd ]
              ->
              begin
                match old_fd with
                    None -> ()
                  | Some fd -> process_close p fd
              end;
              process_fopen p fd ev_seq filename rights

            | "fdopen",
                [ Arg_FILE (Some new_fd); Arg_errno _ ],
                [ Arg_long old_fd; Arg_string (Some rights) ] ->
              () (* nothing to do, rights are not changed, fd is not changed *)

            | ( "open" | "__open" | "__open2"
                  | "open64" | "__open64" | "__open64_2"
            ),
              [ Arg_long fd; Arg_errno _ ],
              [ Arg_string (Some filename); Arg_long flags; Arg_long mode ] ->
              if fd >= 0 then
                process_open p fd ev_seq filename flags

            | ( "creat"),
              [ Arg_long fd; Arg_errno _ ],
              [ Arg_string (Some filename); Arg_long mode ] ->
              if fd >= 0 then
                process_file p fd ev_seq filename OpenWrite

            | "opendir",
                [ Arg_DIR (Some fd); Arg_errno _ ],
                [ Arg_string (Some filename) ] ->
              process_file p fd ev_seq filename OpenRead

            | "opendirat",
                  [ Arg_DIR (Some fd); Arg_errno _ ],
                  [ Arg_long dirfd; Arg_string (Some filename) ] ->
              let filename = filename_at p dirfd filename in
              process_file p fd ev_seq filename OpenRead

            | ( "openat" | "__openat_2"
                  | "openat64" | "__openat64_2"
            ),
              [ Arg_long fd; Arg_errno _ ],
              [ Arg_long dirfd;
                Arg_string (Some filename);
                Arg_long flags;
                Arg_long mode ] ->
              if fd >= 0 then
                let filename = filename_at p dirfd filename in
                process_open p fd ev_seq filename flags

            | ( "unlink" | "remove" ),
              [ Arg_long res; Arg_errno _],
              [ Arg_string (Some filename) ] ->
              if res >= 0 then
                process_file p (-100) ev_seq filename Remove

            | ( "rmdir" ),
              [ Arg_long res; Arg_errno _],
              [ Arg_string (Some filename) ] ->
              if res >= 0 then
                process_file p (-100) ev_seq filename Remove

            | ( "mkdir" ),
              [ Arg_long res; Arg_errno _],
              [ Arg_string (Some filename); Arg_long _ ] ->
              if res >= 0 then
                process_file p (-100) ev_seq filename OpenWrite

            | "mkdir", _, _ -> assert false

            | ( "rename" ),
              [ Arg_long res; Arg_errno _],
              [ Arg_string (Some old_filename);
                Arg_string (Some new_filename) ] ->
              if res = 0 then begin
                process_file p (-100) ev_seq old_filename Remove;
                process_file p (-100) ev_seq new_filename OpenWrite;
              end

            | "unlinkat",
              [ Arg_long res; Arg_errno _],
                [ Arg_long dirfd; Arg_string (Some filename); Arg_long flags ] ->
              if res >= 0 then
                let filename = filename_at p dirfd filename in
                process_file p (-100) ev_seq filename Remove

            | "dup",
              [ Arg_long new_fd; Arg_errno _ ],
              [ Arg_long old_fd ] ->
              if new_fd >= 0 then
                process_dup p old_fd new_fd

            | "dup2",
              [ Arg_long new_fd; Arg_errno _ ],
              [ Arg_long old_fd; Arg_long _ ] ->
              if new_fd >= 0 then
                process_dup p old_fd new_fd

            | "dup3",
              [ Arg_long new_fd; Arg_errno _ ],
              [ Arg_long old_fd; Arg_long _; Arg_long _ ] ->
              if new_fd >= 0 then
                process_dup p old_fd new_fd

            | "fcntl",
              [ Arg_long new_fd; Arg_errno _ ],
              [ Arg_long old_fd;
                Arg_long (0 (* F_DUPFD *) | 1030 (* F_DUPFD_CLOEXEC *))  ] ->
              if new_fd >= 0 then
                process_dup p old_fd new_fd

            | "close", _, [ Arg_long fd ]
            | "fclose", _,[ Arg_FILE (Some fd) ]
            | "closedir", _,[ Arg_DIR (Some fd) ]
              ->
              process_close p fd



            | _ -> ()
        end
      | AfterCallMsg _, _ -> () (* probably a fork *)
  ) file;

  Hashtbl.iter (fun _ p ->
    p.proc_actions <- List.rev p.proc_actions;
    p.proc_events <- List.rev p.proc_events
  ) processes;

  let add_file fi all_files =
      try
        let global_fi = StringMap.find fi.file_name !all_files in
        (match fi.file_read with None -> () | Some (first_seq, last_seq) ->
          file_update_read global_fi first_seq;
          file_update_read global_fi last_seq;
        );
        (match fi.file_write with None -> () | Some (first_seq, last_seq) ->
          file_update_write global_fi first_seq;
          file_update_write global_fi last_seq);
        (match fi.file_remove with None -> () | Some (first_seq, last_seq) ->
          file_update_remove global_fi first_seq;
          file_update_remove global_fi last_seq;
        );
      with Not_found ->
        all_files := StringMap.add fi.file_name
          { fi with file_read = fi.file_read } !all_files
  in
  let rec compute_fds p =
    let all_files = ref StringMap.empty in
    StringMap.iter (fun _ fi ->
      add_file fi all_files
    ) p.proc_files;
    List.iter (fun action ->
      match action with
          ProcActionFork newp ->
            compute_fds newp;
            StringMap.iter
              (fun _ fi -> add_file fi all_files) newp.proc_files_rec
        | _ -> ()
    ) p.proc_actions;
    p.proc_files_rec <- !all_files
  in
  Hashtbl.iter (fun _ p ->
    match p.proc_father with
        Some _ -> ()
      | None ->
        if !just_top && p.proc_pid <> 0 then begin
          all_pids := false;
          pids := IntSet.add p.proc_pid !pids;
        end;
        compute_fds p
  ) processes;


