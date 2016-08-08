(*
  TODO:
  * Limit the size of hashing for very long files
    (same hash on 1 MB + same size => high probability of same file)
  * Add an option to force override of files
  * Add an option to move instead of hardlinking

*)

open StringCompat

module StringMap = Map.Make(String)

type db = {
  files_by_size : (int, size_node) Hashtbl.t;
  mutable hash_by_file : (int * int, Digest.t) Hashtbl.t;
}

and size_node = {
  mutable waiting_for_hash : string list;
  mutable files_by_hash : string StringMap.t;
}

let nerrors = ref 0
let nhash_computed = ref 0
let nbytes_copied = ref 0
let nlink_created = ref 0
let nwarnings = ref 0

let digest_file file =
  incr nhash_computed;
  Digest.file file

let get_hash h filename =
  try
    let st = Unix.lstat filename in
    let key =  (st.Unix.st_dev, st.Unix.st_ino) in
    try
      Some (Hashtbl.find h.hash_by_file key)
    with Not_found ->
      let d = digest_file filename in
      incr nhash_computed;
      Hashtbl.add h.hash_by_file key d;
      Some d
  with e ->
    Printf.eprintf "Could not compute hash of file %S\n%!" filename;
    Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
    None

let empty_db () = {
  files_by_size = Hashtbl.create 1111;
  hash_by_file = Hashtbl.create 1111;
}

let get_size_node h size =
  try
    Hashtbl.find h.files_by_size size
  with Not_found ->
    let sn = {
      waiting_for_hash = [];
      files_by_hash = StringMap.empty;
    } in
    Hashtbl.add h.files_by_size size sn;
    sn

let rec iter_dir ?(recdir=true) f dir =
  let files = try
    Sys.readdir dir
  with e ->
    Printf.eprintf "Error: could not read dir %S\n%!" dir;
    Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
    [||]
  in
  Array.iter (fun basename ->
    let filename = Filename.concat dir basename in
    if recdir && Sys.is_directory filename then
      iter_dir ~recdir f filename
    else
      f filename
  ) files

let rec safe_mkdir filename =
  if not (Sys.file_exists filename) then begin
    safe_mkdir (Filename.dirname filename);
    Unix.mkdir filename 0o755
  end

let copy_file src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else
      begin
        output oc buff 0 n;
        nbytes_copied := !nbytes_copied + n;
        copy();
      end
  in copy();
  close_in ic;
  close_out oc

let arg_list = Arg.align [
  ]

let args = ref []
let arg_anon f = args := f :: !args

let arg_usage = String.concat "\n" [
    "ocp-cpbackup SOURCES DEST"
  ]

let _ =
  Arg.parse arg_list arg_anon arg_usage;
  match !args with
  | [] | [_] ->
    Arg.usage arg_list arg_usage;
    exit 2
  | dstdir :: sources ->

    let dstdir = if Filename.is_relative dstdir then
        Filename.concat (Unix.getcwd()) dstdir
      else dstdir in

    if not (Sys.file_exists dstdir && Sys.is_directory dstdir) then begin
      Printf.eprintf "Error: destination dir must exists\n%!" ;
      exit 2;
    end;

    let h = empty_db () in

    iter_dir (fun filename ->
      try
        let st = Unix.lstat filename in
        let size = st.Unix.st_size in
        try
          let sn = Hashtbl.find h.files_by_size size in
          sn.waiting_for_hash <- filename :: sn.waiting_for_hash
        with Not_found ->
          let sn = {
            waiting_for_hash = [ filename ];
            files_by_hash = StringMap.empty;
          } in
          Hashtbl.add h.files_by_size size sn
      with e ->
        Printf.eprintf "Warning: could not register file %S\n%!" filename;
        Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
        incr nwarnings
    ) dstdir;

    let rec iter_copy srcdir dstdir =
      let files = try
        Sys.readdir srcdir
      with e ->
        Printf.eprintf "Error: could not read src dir %S\n%!" srcdir;
        Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
        [||]
      in
      Array.iter (fun basename ->
        let srcname = Filename.concat srcdir basename in
        let dstname = Filename.concat dstdir basename in
        try
          let st = Unix.lstat srcname in
          match st.Unix.st_kind with
          | Unix.S_DIR ->
            iter_copy
              srcname
              dstname
          | Unix.S_REG ->
            let size = st.Unix.st_size in
            begin try
              (*              let sn = get_size_node h size in *)
              let sn = Hashtbl.find h.files_by_size size in
              let d = digest_file srcname in
              incr nhash_computed;
              let fn =
                try
                  StringMap.find d sn.files_by_hash
                with Not_found ->
                  let rec iter_queue () =
                    match sn.waiting_for_hash with
                    | [] -> raise Not_found
                    | fn :: tail ->
                      sn.waiting_for_hash <- tail;
                      let d' = get_hash h fn in
                      match d' with
                      | None ->
                        Printf.eprintf "Warning: could not compute hash of %S\n%!" fn;
                        incr nwarnings;
                        iter_queue ()
                      | Some d' ->
                        sn.files_by_hash <- StringMap.add d' fn sn.files_by_hash;
                        if d = d' then fn else iter_queue ()
                  in
                  iter_queue ()
              in
              if Sys.file_exists dstname then begin
                let d' = get_hash h dstname in
                match d' with
                | None ->
                  Printf.eprintf "Error: %S exists as %S, but %S already exists and is unreadable\n%!" srcname fn dstname;
                  incr nerrors;
                | Some d' ->
                if d <> d' then begin
                  Printf.eprintf "Error: %S exists as %S, but %S already exists and differs\n%!" srcname fn dstname;
                  incr nerrors;
                end
              end else begin
                Printf.eprintf "For %S, creating a link from %S to %S\n%!"
                  srcname fn dstname;
                try
                  safe_mkdir dstdir;
                  Unix.link fn dstname;
                  incr nlink_created;
                with e ->
                  Printf.eprintf "Error: could not create file %S by hardlink from %S\n%!" dstname fn;
                  Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
                  incr nerrors;

              end
            with Not_found ->
              let sn = get_size_node h size in
              if not (Sys.file_exists dstname) then begin
                sn.waiting_for_hash <- dstname ::
                    sn.waiting_for_hash;
                safe_mkdir dstdir;
                copy_file srcname dstname;
              end else begin
                let d' = get_hash h dstname in
                match d' with
                | None ->
                  Printf.eprintf "Error: could not backup %S, because %S already exists and is not readable\n%!" srcname dstname;
                  incr nerrors;
                | Some d' ->
                  let d = digest_file srcname in
                  if d <> d' then begin
                    Printf.eprintf "Error: could not backup %S, because %S already exists and differs\n%!" srcname dstname;
                    incr nerrors;
                  end
              end
            end
          | _ -> assert false
        with e ->
          Printf.eprintf "Error: could not stat src file %S\n%!" srcname;
          Printf.eprintf "Exception: %S\n%!" (Printexc.to_string e);
          incr nerrors;
      ) files
    in

    let sources = List.rev sources in
    List.iter (fun srcdir ->
      let dstdir =
        match Filename.basename srcdir with
        | "." | "" -> dstdir
        | basename ->
          Filename.concat dstdir basename
      in
      iter_copy srcdir dstdir
    ) sources;

    Printf.printf "%d bytes copied (%d hashes computed, %d hard links created)\n%!" !nbytes_copied !nhash_computed !nlink_created;
    if !nerrors + !nwarnings > 0 then begin
      Printf.printf "%d errors occurred (%d warnings)\n%!" !nerrors !nwarnings;
    end;
    if !nerrors > 0 then begin
      exit 2
    end;
    ()
