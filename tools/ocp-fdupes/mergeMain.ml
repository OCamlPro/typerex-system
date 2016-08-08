
(* Fabrice: the same as "fdupes", but this tool ignores some directories
  (like ".svn", ".git", "Private" and ".Private"). For ".git", it goes
  directly into ".git/objects".

  It also takes Unix permissions into account:
  * Files are only hardlinked if they share the same r/x permissions
  * Hardlinked files are created as read-only to avoid modification.
 *)

(* TODO:
   * Fix removal of "already hardlinked" files
   * Add a -revert option to revert hardlinks
   * Add an option -ocaml to remove:
     * _build directories
     * _obuild/*/ directories
     * *~ files
   * Use a different database:
     * per partition
     * at .svn/ frontiers

*)

let break = ref false
let _ =
  Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> break := true))

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type done_tree = {
  mutable all_done : bool;
  mutable already_done : done_tree StringMap.t;
}

type db = {
  files_by_size : (int, size_node) Hashtbl.t;
  mutable hash_by_file : (key, Digest.t) Hashtbl.t;
  done_tree : done_tree;
}

and size_node = {
  mutable waiting_for_hash : (file * key) option;
  mutable files_by_hash : (file * key) StringMap.t;
}

and key = int * int

and file = string list

let database_filename = ref "fdupes.db"
let load_database = ref None
let log_file = ref "mergedup.log"
let non_empty = ref true
let fake_only = ref false

let ndup_found = ref 0
let ndup_found_bytes = ref 0
let nhash_computed = ref 0
let nhash_error = ref 0
let nunreadable_dir = ref 0
let nalready_dup = ref 0
let nhardlinked = ref 0
let ntotal_bytes = ref 0
let nerrors = ref 0
let errors = ref []

let filename_of_file file =
  String.concat "/" (List.rev file)

let print_stats () =
  Printf.printf "Dup found: %d\n%!" !ndup_found;
  Printf.printf "Dup found: %d bytes\n%!" !ndup_found_bytes;
  Printf.printf "Total files: %d bytes\n%!" !ntotal_bytes;
  Printf.printf "Hash computed: %d\n" !nhash_computed;
  Printf.printf "Hash errors: %d\n" !nhash_error;
  Printf.printf "Unreadable dirs: %d\n" !nunreadable_dir;
  Printf.printf "Hardlinks: %d\n" !nhardlinked;
  Printf.printf "Already dupped: %d\n" !nalready_dup;
  Printf.printf "ERRORS: %d\n" !nerrors;
  List.iter (fun file ->
    Printf.printf "     %s\n" (filename_of_file file);
  ) !errors;

(*
  List.iter (fun (file, old_file) ->
    Printf.printf "   %s\n" (filename_of_file file);
  ) !already_dupped;
*)
  Printf.printf "%!";
  ()

let homedir = Sys.getenv "HOME"

let dup_ignore =
  let set = ref StringSet.empty in
  List.iter (fun file ->
    set := StringSet.add file !set)
    [
      ".svn";
      ".git";
      "Private";
      ".Private";
    ];

  begin try
    let ic = open_in (Filename.concat homedir ".ocp/fdupes.ignore") in
    try
      while true do
        let line = input_line ic in
        if line <> "" then
          if line.[0] = '-' then
            set := StringSet.remove
                (String.sub line 1 (String.length line -1)) !set
          else
            set := StringSet.add line !set
      done
    with _ -> close_in ic
  with _ -> ()
  end;

  set

let log = ref None

let get_key st = (st.Unix.st_dev, st.Unix.st_ino)

let digest_file file =
  incr nhash_computed;
  let st = Unix.lstat file in
  let mode = st.Unix.st_perm land 0o77555 in
  Digest.file file ^ string_of_int mode

let get_hash db filename key =
  try
    try
      Some (Hashtbl.find db.hash_by_file key)
    with Not_found ->
      let d = digest_file filename in
      Hashtbl.add db.hash_by_file key d;
      Some d
  with e ->
    Printf.eprintf "Could not compute hash of file %S\n%!" filename;
    Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
    None

let found_dup filename file old_file st_size =
(*  Printf.eprintf "dup: %s -> %s\n%!" filename (filename_of_file old_file); *)
  incr ndup_found;
  ndup_found_bytes := !ndup_found_bytes + st_size;

  if not !fake_only then begin
    let tmp_filename = filename ^ ".hardlinking" in
    let new_filename = filename ^ ".hardlink" in
    let old_filename = filename_of_file old_file in
    begin
      let oc =
        match !log with
          None ->
          let oc = if !log_file = "-" then stdout else open_out !log_file in
          log := Some oc;
          oc
        | Some oc -> oc
      in
      Printf.fprintf oc "%s\n" filename;
    end;
    try
      Unix.link old_filename new_filename;
      let st = Unix.lstat filename in
      let mode = st.Unix.st_perm in
(*      Printf.eprintf "old mode: %o\n%!" mode; *)
      let mode = mode land 0o77555 in
(*      Printf.eprintf "new mode: %o\n%!" mode; *)
      Sys.rename filename tmp_filename;
      Sys.rename new_filename filename;
      Unix.chmod filename mode;
      Sys.remove tmp_filename
    with _ ->
      incr nerrors;
      errors := file :: !errors
  end


let check_file db filename file st =
  let st_size =  st.Unix.st_size in
  ntotal_bytes := st_size + !ntotal_bytes;
  try
    let key = get_key st in
    let node = Hashtbl.find db.files_by_size st_size in
    match get_hash db filename key with
    | None -> incr nhash_error
    | Some hash ->
      try
        let (old_file, old_key) = StringMap.find hash node.files_by_hash in
        if file = old_file then () else
        if old_key = key then
          incr nhardlinked
        else
        if st.Unix.st_nlink = 1 then
          found_dup filename file old_file st_size
        else begin
          incr nalready_dup;
(*          already_dupped := (file, old_file) :: !already_dupped *)
        end
      with Not_found ->
        match node.waiting_for_hash with
        | None ->
          node.files_by_hash <- StringMap.add
              hash (file, key) node.files_by_hash
        | Some (old_file, old_key) ->
          node.waiting_for_hash <- None;
        if old_key = key then begin
          incr nhardlinked;
          node.files_by_hash <- StringMap.add
              hash (file, key) node.files_by_hash;
        end else
          let old_filename = filename_of_file old_file in
          match get_hash db old_filename old_key with
          | None ->
            incr nhash_error;
            node.files_by_hash <- StringMap.add
                hash (file, key) node.files_by_hash;
          | Some old_hash ->
            if old_hash <> hash then begin
              node.files_by_hash <- StringMap.add
                  old_hash (old_file, old_key) node.files_by_hash;
              node.files_by_hash <- StringMap.add
                  hash (file, key) node.files_by_hash;
            end else
              let old_st = Unix.lstat old_filename in
              match st.Unix.st_nlink, old_st.Unix.st_nlink with
              | _, 1 ->
                node.files_by_hash <- StringMap.add
                    hash (file, key) node.files_by_hash;
                found_dup old_filename old_file file st_size
              | 1, _ ->
                node.files_by_hash <- StringMap.add
                    old_hash (old_file, old_key) node.files_by_hash;
                found_dup filename file old_file st_size
              | n, old_n ->
                incr nalready_dup;
(*                already_dupped := (file, old_file) :: !already_dupped; *)
                if n > old_n then
                  node.files_by_hash <- StringMap.add
                      hash (file, key) node.files_by_hash
                else
                  node.files_by_hash <- StringMap.add
                      old_hash (old_file, old_key) node.files_by_hash;
  with Not_found ->
    let node = {
      waiting_for_hash = Some (file, get_key st);
      files_by_hash = StringMap.empty;
    } in
    Hashtbl.add db.files_by_size st_size node



let load_db =
  let db = ref None in
  function () ->
    match !db with
    | Some db -> db
    | None ->
      let v =
        match !load_database with
        | None ->
          {
            files_by_size = Hashtbl.create 1111;
            hash_by_file = Hashtbl.create 1111;
            done_tree = {
              all_done = false;
              already_done = StringMap.empty;
            };
          }
        | Some filename ->
          let ic = open_in_bin filename in
          let db = input_value ic in
          close_in ic;

          Printf.eprintf "Starting at:\n";
          let rec iter indent node =
            StringMap.iter (fun basename node ->
              for i = 0 to indent do
                Printf.eprintf " ";
              done;
              Printf.eprintf "%s" basename;
              if node.all_done then
                Printf.eprintf " DONE\n%!"
              else begin
                Printf.eprintf " Partial:\n";
                iter (indent+2) node
              end
            ) node.already_done
          in
          iter 0 db.done_tree;


          db
      in
      db := Some v;
      v

let save_db () =
  let db = load_db () in
  Printf.eprintf "Saving state in %S\n%!" !database_filename;
  let oc = open_out_bin !database_filename in
  output_value oc db;
  close_out oc

exception Done
let rec found_done dir node =
  if node.all_done then raise Done else
    match dir with
      [] -> node
    | basename :: dir ->
      let node = try
                   StringMap.find basename node.already_done
        with Not_found ->
          let new_n = {
            all_done = false;
            already_done = StringMap.empty;
          } in
          node.already_done <- StringMap.add basename new_n node.already_done;
          new_n
      in
      found_done dir node

let nstats = ref 0

let merge filename =
  if !break then raise Sys.Break;
  let db = load_db () in
  let filename = if Filename.is_implicit filename ||
      Filename.is_relative filename then
      Filename.concat (Unix.getcwd()) filename
    else filename in
  let base_st = Unix.lstat filename in

  let rec iter filename =
    let dirname = Filename.dirname filename in
    let basename = Filename.basename filename in
    (*
      Printf.eprintf "dirname = %S\n%!" dirname;
      Printf.eprintf "basename = %S\n%!" basename;
    *)
    if basename = "/" then
      [ "" ]
    else
      basename :: (iter dirname)
  in
  let dir = iter filename in

  let rec iter_files dirname dir node =
    if !break then raise Sys.Break;
    if not node.all_done then
      let files = try
                    let files = Sys.readdir dirname in
                    Array.sort compare files;
                    files
        with _ ->
          incr nunreadable_dir;
          [||]
      in
      let t0 = Unix.gettimeofday () in
      let prev_nstats = !nstats in
      let prev_ndups = !ndup_found in
      let prev_ndups_bytes = !ndup_found_bytes in
      Array.iter (fun basename ->
        if !break then raise Sys.Break;
        let basename =
          if basename = ".git" then
            (* go directly in .git/objets *)
            let new_basename = ".git/objects" in
            let filename = Filename.concat dirname new_basename in
            if Sys.file_exists filename then
              new_basename
            else
              basename
          else
            basename
        in
        if not (StringSet.mem basename !dup_ignore) then
          let fullfile = basename :: dir in
          let filename = Filename.concat dirname basename in
          let st = Unix.lstat filename in
          incr nstats;
          (* Check to not cross file-systems *)
          if st.Unix.st_dev = base_st.Unix.st_dev then
            match st.Unix.st_kind with
            | Unix.S_DIR ->
              begin
                let node = try
                             StringMap.find basename node.already_done
                  with Not_found ->
                    let new_n = {
                      all_done = false;
                      already_done = StringMap.empty;
                    } in
                    node.already_done <-
                      StringMap.add basename new_n node.already_done;
                    new_n
                in
                iter_files filename fullfile node
              end
            | Unix.S_REG ->
              if st.Unix.st_size > 0 then
                check_file db filename fullfile st
            | _ -> ()
      ) files;
      node.all_done <- true;
      node.already_done <- StringMap.empty;
      let t1 = Unix.gettimeofday () in
      if t1 -. t0 > 5. then
        Printf.eprintf "%s: %.1fs (%d/%d dups)\n%!" dirname (t1 -. t0)
          (!ndup_found - prev_ndups) (!nstats - prev_nstats)
  in
  try
    let node = found_done (List.rev dir) db.done_tree in
    iter_files filename dir node;
    node.all_done <- true;
    node.already_done <- StringMap.empty
  with Done -> ()

let arg_list = Arg.align [
    "-k", Arg.Set fake_only, " Don't do anything, just check";
    "-ignore", Arg.String (fun s ->
      dup_ignore := StringSet.add s !dup_ignore), "BASENAME Ignore this file or directory";
    "-no-ignore", Arg.String (fun s ->
      dup_ignore := StringSet.remove s !dup_ignore), "BASENAME Don't ignore this file or directory";
    "-log", Arg.String ( (:=) log_file), "FILE.log The file where operations should be logged";
    "-load", Arg.String (fun s -> load_database := Some s),
    "FILE Load database from file";
  ]

let arg_usage = String.concat "\n"
    [ "ocp-fdupes [OPTIONS] dirs";
      "  Find duplicate files within the listed directories, and replace them";
      "  by hardlinks. [ocp-fdupes] does not use a database, it is always local";
      "  to specified directories.";
    ]

let _ =
  try
    Arg.parse arg_list merge arg_usage;
    save_db ();
    print_stats ();
    match !log with
    | None -> ()
    | Some oc -> close_out oc
  with Sys.Break ->
    Printf.eprintf "ocp-fdupes interrupted\n%!";
    save_db ()


(*
FDUPES(1)                                                            FDUPES(1)

NAME
       fdupes - finds duplicate files in a given set of directories

SYNOPSIS
       fdupes [ options ] DIRECTORY ...

DESCRIPTION
       Searches  the  given  path for duplicate files. Such files are found by
       comparing file sizes and MD5 signatures,  followed  by  a  byte-by-byte
       comparison.

OPTIONS
       -r --recurse
              for  every  directory  given  follow  subdirectories encountered
              within

       -R --recurse:
              for each directory given after this option follow subdirectories
              encountered  within  (note the ':' at the end of option; see the
              Examples section below for further explanation)

       -s --symlinks
              follow symlinked directories

       -H --hardlinks
              normally, when two or more files point to  the  same  disk  area
              they are treated as non-duplicates; this option will change this
              behavior

       -n --noempty
              exclude zero-length files from consideration

       -f --omitfirst
              omit the first file in each set of matches

       -A --nohidden
              exclude hidden files from consideration

       -1 --sameline
              list each set of matches on a single line

       -S --size
              show size of duplicate files

       -m --summarize
              summarize duplicate files information

       -q --quiet
              hide progress indicator

       -d --delete
              prompt user for files to  preserve,  deleting  all  others  (see
              CAVEATS below)

       -L --hardlink
              replace  all duplicate files with hardlinks to the first file in
              each set of duplicates

       -N --noprompt
              when used together with --delete, preserve  the  first  file  in
              each  set  of duplicates and delete the others without prompting
              the user

       -D --debug
              provide debugging information

       -v --version
              display fdupes version

       -h --help
              displays help

SEE ALSO
       md5sum(1)

NOTES
       Unless -1 or  --sameline  is  specified,  duplicate  files  are  listed
       together  in groups, each file displayed on a separate line. The groups
       are then separated from each other by blank lines.

       When -1 or --sameline is specified,  spaces  and  backslash  characters
       (\) appearing in a filename are preceded by a backslash character.



*)
