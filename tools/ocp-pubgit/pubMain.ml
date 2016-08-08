module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let is_directory s =
  try
    (Unix.lstat s).Unix.st_kind = Unix.S_DIR
  with _ -> false

let packages_arg = ref []
let message_arg = ref None
let version_arg = ref None
let list_arg = ref false
let all_arg = ref false
let diff_arg = ref false
let verbose_arg = ref false
let import_arg = ref false
let export_arg = ref false

let arg_list = Arg.align
    [
      "-m", Arg.String (fun s -> message_arg := Some s), "MESSAGE Define the commit message";
      "-t", Arg.String (fun s -> version_arg := Some s), "VERSION Define the version to tag with";
      "-l", Arg.Set list_arg, " List all packages and exit";
      "-a", Arg.Set all_arg, " Push all packages";
      "-d", Arg.Set diff_arg, " Print a diff";
      "-v", Arg.Set verbose_arg, " Print debug messages";
      "-i", Arg.Set import_arg, " Import specified packages (pull and copy, no commit)";
      "-e", Arg.Set export_arg, " Export specified packages (commit and push)";
    ]

let arg_anon s = packages_arg := s :: !packages_arg
let arg_usage = "ocp-publish [OPTIONS] [PACKAGES]: push a list of packages from a GIT private repository to a set of GIT public repositories."

let rec go_to_root_dir marker =
  if not (Sys.file_exists marker) then begin
    let st_current = Unix.lstat "." in
    let st_previous = Unix.lstat ".." in
    if st_current = st_previous then begin
      Printf.eprintf "Error: could not find marker %S\n%!" marker;
      exit 2
    end;
    Unix.chdir "..";
    go_to_root_dir marker
  end

let lines_of_file filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := (input_line ic) :: !lines
    done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !lines

let pubimport_basename = ".pubimport"
let pubignore_basename = ".pubignore"
let pubconfig_basename = ".pubconfig"
let pubdir_basename = ".pub"
let gitdir_basename = ".git"

let ignored_files = [
  pubimport_basename;
  pubignore_basename;
  pubconfig_basename;
  pubdir_basename;
  gitdir_basename;
]

type content =
    Dir of (string * content) list
  | File

  let files_of_content content =
    let files = ref StringSet.empty in
    let rec iter dir content =
      List.iter (fun (basename, kind) ->
        match kind with
        | File ->
          let file = String.concat "/" (dir @ [basename]) in
          (* Printf.eprintf "%s: %s\n%!" name file; *)
          files := StringSet.add file !files;
        | Dir content ->
          iter (dir @ [basename]) content
      ) content
    in
    iter [] content;
    !files

let command s =
  let error = Sys.command s in
  if error <> 0 then begin
    Printf.eprintf "Error: command %S failed with status %d\n%!" s error;
    exit 2
  end

let git_files_of_index filename =
  let index = GitIndex.of_file filename in
  let git_files = ref StringSet.empty in
  Array.iter (fun e ->
    git_files := StringSet.add e.GitIndex.name !git_files;
  ) index.GitIndex.entries;
  let git_files = !git_files in
  git_files

type package = {
  name : string;
  reldir : string;
  content : (string * content) list;
  git : string;
  export : bool;
  branch : string;
}

let _ =
  Arg.parse arg_list arg_anon arg_usage;
  packages_arg := List.rev !packages_arg;

  let marker = ".git" in
  Printf.eprintf "Looking for marker %S\n%!" marker;
  go_to_root_dir marker;
  let curdir = Unix.getcwd () in
  Printf.eprintf "Moved to root directory %S\n%!" curdir;

  let git_files = git_files_of_index ".git/index" in

  let ignored = List.fold_left (fun acc ele ->
      StringSet.add ele acc
    ) StringSet.empty ignored_files in

  let packages = ref StringMap.empty in

  let rec scan dir =
    let reldir = String.concat "/" dir in
    (*    Printf.eprintf "scan %S\n%!" reldir; *)
    let fulldir = Filename.concat curdir reldir in
    let ignored_files =
      let ignored_file = Filename.concat fulldir pubignore_basename in
      if Sys.file_exists ignored_file then
        lines_of_file ignored_file
      else
        []
    in
    let ignored =
      List.fold_left (fun acc ele -> StringSet.add ele acc)
        ignored ignored_files
    in
    let files = Sys.readdir fulldir in
    let content = ref [] in
    Array.iter (fun basename ->
      let relfile = Filename.concat reldir basename in
      if not (StringSet.mem basename ignored) then
        let filename = Filename.concat fulldir basename in
        if is_directory filename then begin
          let dirfiles = scan (dir @ [basename] ) in
          if dirfiles <> [] then
            content := (basename, Dir dirfiles) :: !content
        end else
        if StringSet.mem relfile git_files then
          content := (basename, File) :: !content
        else begin
          if !verbose_arg then
            Printf.eprintf "Ignoring non-git %s\n%!" relfile
        end
      else begin
        if !verbose_arg then
          Printf.eprintf "Ignoring non-pub %s\n%!" relfile;
      end
    ) files;

    let config_file = Filename.concat fulldir pubconfig_basename in
    let content = !content in
    if Sys.file_exists config_file then begin
      let lines = lines_of_file config_file in
      let name = ref None in
      let git = ref None in
      let export = ref false in
      let branch = ref "master" in
      List.iter (fun line ->
        let ident, v = OcpString.cut_at line ':' in
        let v = OcpString.skip_chars v " \t" in
        match ident with
        | "" -> ()
        | "name" -> name := Some v
        | "export" ->
          export := (match v with
              "true" | "yes" -> true
            | "false" | "no" -> false
            | _ ->
              Printf.eprintf "Error: bad boolean value %S for %S in %S\n%!"
                v ident config_file;
              exit 2);
        | "git" -> git := Some v
        | "branch" -> branch := v
        | _ ->
          Printf.eprintf "Error: unknown config name %S in %S\n%!"
            ident config_file; exit 2
      ) lines;
      let git = match !git with
          None ->
          Printf.eprintf "Error: no git url in %S\n%!"
            config_file; exit 2
        | Some git -> git
      in
      let export = !export in
      let branch = !branch in
      match !name with
      | None ->
        Printf.eprintf "Error: no package name in %S\n%!"
          config_file; exit 2
      | Some name ->
        try
          let p = StringMap.find name !packages in
          Printf.eprintf "Error: Package %S is present in two directories:\n"
            name;
          Printf.eprintf "\t%S\n\t%S\n%!" reldir p.reldir;
          exit 2
        with Not_found ->
          packages := StringMap.add name { name; reldir; content; git; export; branch } !packages
    end;
    content
  in
  let (_ : (string * content) list) = scan [] in

  let packages = !packages in
  if !list_arg then begin
    Printf.printf "%d available packages:" (StringMap.cardinal packages);
    StringMap.iter (fun name _ -> Printf.printf " %S" name) packages;
    Printf.printf "\n";
    StringMap.iter (fun name p ->
      Printf.printf "\t%S in %S\n" name p.reldir) packages;
    Printf.printf "%!";
    exit 0;
  end;


  if !packages_arg = [] then begin
    if !all_arg then
      StringMap.iter (fun name p ->
        if !import_arg || (p.export || not !export_arg) then
          packages_arg := name :: !packages_arg) packages
    else begin
      Printf.eprintf "Error: no package specified (use -a for all)\n%!";
      exit 2
    end;
  end;

  let packages = List.map (fun name ->
      try
        (name, StringMap.find name packages)
      with Not_found ->
        Printf.eprintf "Error: could not find package %S\n%!" name;
        exit 2
    ) !packages_arg in

  begin try
    Unix.chdir ".pub"
  with _ ->
    Unix.mkdir ".pub" 0o755;
    Unix.chdir ".pub"
  end;
  Unix.chdir curdir;

  List.iter (fun (name, p) ->
    Unix.chdir curdir;
    let gitdir = String.concat "/" [".pub"; name ] in
    let gitfile = String.concat "/" [".pub"; name; ".git" ] in
    if Sys.file_exists gitfile then begin
      Printf.eprintf "Updating %s\n%!" gitdir;
      Unix.chdir gitdir;

      Printf.kprintf command "git checkout %s" p.branch;
      command "git clean -fdx";
      command "git fetch origin";
      Printf.kprintf command "git reset origin/%s --hard" p.branch;

    end else begin

      if Sys.file_exists gitdir then begin
        Printf.eprintf "Error: non-git repo %s exists.\n%!" gitdir;
        exit 2;
      end;

      Printf.kprintf command "git clone %s %s" p.git gitdir;
      Unix.chdir gitdir;
      Printf.kprintf command "git checkout %s" p.branch;

    end
  ) packages;

  let cp f1 f2 =
    Printf.kprintf command "mkdir -p %S" (Filename.dirname f2);
    Printf.kprintf command "cp %S %S" f1 f2
  in

  let export_packages packages =

    let message =
      match !message_arg with
        None ->
        if not !list_arg then begin
          Printf.eprintf "Error: you must provide a commit message with -m\n%!";
          exit 2;
        end else ""
      | Some msg -> msg
    in

    Unix.chdir curdir;
    List.iter (fun (name, p) ->
      let gitdir = String.concat "/" [".pub"; name ] in
      let full_gitdir = Filename.concat curdir gitdir in
      let gitindex = String.concat "/" [ gitdir; ".git"; "index" ] in
      Unix.chdir curdir;
      let old_files = git_files_of_index gitindex in
      let new_files = files_of_content p.content in
      StringSet.iter (fun s ->
        if not (StringSet.mem s old_files) then begin
          Printf.eprintf "%s: adding file %S\n%!" name s;
          Unix.chdir curdir;
          cp (Filename.concat p.reldir s) (Filename.concat gitdir s);
          Unix.chdir full_gitdir;
          command (Printf.sprintf "git add -f %S" s);
          Unix.chdir curdir;
        end else
        if Sys.command (Printf.sprintf "cmp %S %S"
              (Filename.concat p.reldir s)
              (Filename.concat gitdir s)
          ) <> 0 then begin
          Printf.eprintf "%s: updating file %S\n%!" name s;
          Unix.chdir curdir;
          cp (Filename.concat p.reldir s) (Filename.concat gitdir s);
          Unix.chdir full_gitdir;
          command (Printf.sprintf "git add -f %S" s);
          Unix.chdir curdir;
        end;
      ) new_files;

      StringSet.iter (fun s ->
        if not (StringSet.mem s new_files) then begin
          Printf.eprintf "%s: removing file %S\n%!" name s;
          Unix.chdir full_gitdir;
          command (Printf.sprintf "git rm -f %S" s);
          Unix.chdir curdir;
        end
      ) old_files;

      begin
        Unix.chdir full_gitdir;
        command "git status";
      end;

      if !diff_arg then begin
        Unix.chdir full_gitdir;
        Printf.kprintf command "git diff %s" p.branch;
      end;

      command (Printf.sprintf "git commit -m %S" message)
    ) packages;



    List.iter (fun (name, p) ->
      let gitdir = String.concat "/" [".pub"; name ] in
      let full_gitdir = Filename.concat curdir gitdir in
(*
    let gitindex = String.concat "/" [ gitdir; ".git"; "index" ] in
*)
      Unix.chdir full_gitdir;
      Printf.kprintf command "git push origin %s" p.branch
    ) packages;
  in

  let import_packages packages =

    Unix.chdir curdir;

    List.iter (fun (name, p) ->
      let gitdir = String.concat "/" [".pub"; name ] in
      let full_gitdir = Filename.concat curdir gitdir in
      let gitindex = String.concat "/" [ gitdir; ".git"; "index" ] in
      Unix.chdir curdir;

      let new_files = git_files_of_index gitindex in
      let old_files = files_of_content p.content in

      StringSet.iter (fun s ->
        if not (StringSet.mem s old_files) then begin
          Printf.eprintf "%s: adding file %S\n%!" name s;
          Unix.chdir curdir;
          cp (Filename.concat gitdir s) (Filename.concat p.reldir s);
          Unix.chdir p.reldir;
          Printf.kprintf command "git add -f %S" s;
          Unix.chdir curdir;
        end else
        if  Printf.kprintf Sys.command "cmp %S %S"
              (Filename.concat p.reldir s)
              (Filename.concat gitdir s)
           <> 0 then begin
          Printf.eprintf "%s: updating file %S\n%!" name s;
          Unix.chdir curdir;
          cp (Filename.concat gitdir s) (Filename.concat p.reldir s);
          Unix.chdir p.reldir;
          Printf.kprintf command "git add -f %S" s;
          Unix.chdir curdir;
        end;
      ) new_files;

      StringSet.iter (fun s ->
        if not (StringSet.mem s new_files) then begin
          Printf.eprintf "%s: removing file %S\n%!" name s;
          Unix.chdir p.reldir;
          Printf.kprintf command "git rm -f %S" s;
          Unix.chdir curdir;
        end
      ) old_files;

      Unix.chdir full_gitdir;
      Printf.kprintf command
        "git rev-list --max-count=1 HEAD > %s/%s/.pubcommit"
        curdir p.reldir;
      Unix.chdir curdir;
      Printf.kprintf command "git add -f %s/.pubcommit" p.reldir;
    ) packages;


    begin
      Unix.chdir curdir;
      command "git status";
    end;

    if !diff_arg then begin
      Unix.chdir curdir;
      command "git diff";
    end;

  in


  if !import_arg then
    import_packages packages
  else
  if !export_arg then
    export_packages packages


