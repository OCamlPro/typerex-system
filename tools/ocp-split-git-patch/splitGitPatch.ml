(******************************************************************************)
(*                                                                            *)
(*                         TypeRex Development Tools                          *)
(*                                                                            *)
(* Copyright OCamlPro/INRIA 2014.                                             *)
(* This file is distributed under the terms of the GNU Public Licence (GPL).  *)
(*                                                                            *)
(******************************************************************************)

let starts_with = OcpString.starts_with
let split = OcpString.split

let diff_git = "diff "


(*
let input_line ic =
  let line = input_line ic in
  Printf.eprintf "input_line: %s\n%!" line; line
*)

let rec safe_mkdir dirname umask =
  if not (Sys.file_exists dirname) then
    let parent = Filename.dirname dirname in
    if parent <> dirname then safe_mkdir parent umask;
    Unix.mkdir dirname umask

let verbose = ref false
let no_append = ref false
let patch_extension = ref ".patch"
let recursive = ref false

let split_patch patch_filename =
  let filename = ref None in
  let b = Buffer.create 1000 in
  let flush_patch () =
    match !filename with
    | None ->
      Printf.eprintf
        "Error: end of patch file %s reached without finding a patch\n%!"
        patch_filename;
      exit 2
    | Some filename_patch ->
      Printf.eprintf "File %s\n%!" filename_patch;
      filename := None;
      let filename_patch =
        if !recursive then filename_patch else
          OcpString.replace_chars filename_patch
            ['/', "_"] in
      let filename = filename_patch ^ !patch_extension in
      let oc =
        if Sys.file_exists filename then
          if !no_append then
            let rec iter_diff filename i =
              let new_filename = Printf.sprintf "%s.%d" filename i in
              if Sys.file_exists new_filename then
                iter_diff filename (i+1)
              else begin
                if !verbose then
                  Printf.eprintf "Generating diff to %s\n%!" new_filename;
                open_out new_filename
              end
            in
            iter_diff filename 1
          else begin
            if !verbose then
              Printf.eprintf "Appending diff to %s\n%!" filename;
            open_out_gen
              [Open_append;Open_text]
              0o644 filename
          end else begin
            if !verbose then
              Printf.eprintf "Generating diff to %s\n%!" filename;
            safe_mkdir (Filename.dirname filename) 0o755;
            open_out filename
          end in
      output_string oc (Buffer.contents b);
      close_out oc;
      Buffer.clear b
  in

  let ic = open_in patch_filename in
  let rec iter_header () =
    let line = input_line ic in
    if starts_with line diff_git then
      begin_patch line
    else begin
      if !verbose then
        Printf.eprintf "Discarding header: %s\n%!" line;
      iter_header ()
    end

  and begin_patch line =
    match split line ' ' with
    | "diff" :: args ->
      iter_diff_args line args
    | _ ->
      Printf.eprintf "Ill formed line for diff:\n%s\n%!" line;
      exit 2

  and iter_diff_args line args =
    match args with
    | [ orig; dst ] ->
        assert (!filename = None);
        filename := Some dst;
        Buffer.add_string b line;
        Buffer.add_char b '\n';
        iter_patch ()
    | "--git" :: args
    | "-r" :: args
    | "-N" :: args
    | "-w" :: args
    | "-b" :: args
    | "-B" :: args
    | "-C" :: _ :: args
      -> iter_diff_args line args
    | _ ->
      Printf.eprintf "Ill formed line for diff:\n%s\n%!" line;
      exit 2

  and iter_patch () =
    let line = input_line ic in
    if starts_with line diff_git then begin
      flush_patch ();
      begin_patch line
    end else begin
      Buffer.add_string b line;
      Buffer.add_char b '\n';
      iter_patch ()
    end
  in
  try
    iter_header ()
  with End_of_file ->
    flush_patch ();
    close_in ic


(* Parse arguments and start real work *)

let arg_usage = Printf.sprintf
  "%s git_patch*: split diffs from a patch generated with 'git format-patch'"
  (Filename.basename Sys.argv.(0))

let arg_anon = split_patch
let arg_list = Arg.align [
  "-e", Arg.String ((:=) patch_extension), ".EXT Extension to be used for every file";
  "-v", Arg.Set verbose, " Set verbose output";
  "-no-append", Arg.Set no_append,
  " Create a new file instead of appending patches for existing diffs";
  "-r", Arg.Set recursive, " Save patches in their directories";
]

let () =
  Arg.parse arg_list arg_anon arg_usage
