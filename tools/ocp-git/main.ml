open StringCompat
open GitTypes

module Sha1Set = Set.Make(struct
    type t = Sha1.t
    let compare = compare
  end)
module Sha1Map = Map.Make(struct
    type t = Sha1.t
    let compare = compare
  end)

let homedir = try Sys.getenv "HOME" with Not_found -> "/tmp"
let homedir = FileGen.of_string homedir

let rec iter file =
  let git_dir = FileGen.add_basename file ".git" in
  if FileGen.exists git_dir then
    let shared_dir = FileGen.add_basenames homedir [ ".ocp"; "git" ] in
    Git.read_git git_dir shared_dir
  else
  let dirname = FileGen.dirname file in
  if file == dirname then exit 0;
  iter dirname

let repo () =
  try
    iter (FileGen.getcwd ())
  with e ->
    Printf.eprintf "Exception %s\n%!" (Printexc.to_string e);
    exit 2

let arg_usage = Printf.sprintf "%s [options], git arguments" Sys.argv.(0)

let cat_file filename =
  let file = FileGen.of_string filename in
  let s = FileGen.read_file file in
  let s = Zlib.uncompress_string s in
  Printf.printf "%s\n%!" s

let rank_contributors _ =
  let _ = repo () in
  ()


let rec get_all_commits o set tree commits =
  match o.obj_kind with
  | Commit c ->
(*    Printf.printf "  %s added (%d)\n%!" (Sha1.to_hex o.obj_sha1)
      (List.length c.commit_parents); *)
    let merge = match c.commit_parents with
      [] -> false
      | [_] -> false
      | _ -> true
    in
    iter_branch set
      ( (o.obj_sha1, merge, c.commit_parents) :: tree ) commits
  | _ -> assert false

and iter_branch set tree commits =
  match tree with
  | [] -> commits, set
  | (commit, all_parents, []) :: tree ->
(*    Printf.printf "  %s done\n%!" (Sha1.to_hex commit); *)
    iter_branch (Sha1Set.add commit set) tree
      ( (commit, all_parents) :: commits)
  | (commit, all_parents, parent :: parents) :: tree ->
    if not (Sha1Set.mem parent.obj_sha1 set) then
      get_all_commits parent set
        ( (commit, all_parents, parents) :: tree) commits
    else
      iter_branch set ( (commit, all_parents, parents) :: tree) commits

let get_branches master =
  let r = repo () in
  let all_commits = ref Sha1Map.empty in
  let branches =
    StringMap.map (fun br ->
      let commits, set = get_all_commits br.branch_object Sha1Set.empty [] [] in
      List.iter (fun (commit, merge) ->
        if not merge then
          try
            let ref = Sha1Map.find commit !all_commits in
            ref := br.branch_name :: !ref
          with Not_found ->
            all_commits := Sha1Map.add commit (ref [br.branch_name])
                !all_commits
      ) commits;
      (commits, set)
    ) r.git_heads
  in
  let master_commits, master_set = StringMap.find master branches in
  Sha1Map.iter (fun _ list ->
    list := List.sort compare !list
  ) !all_commits;
  Printf.printf "  %s: %d commits\n%!" master (List.length master_commits);
  StringMap.iter (fun name (commits, set) ->
    if name <> master then begin
      let total = List.length commits in
      let common = ref 0 in
      let more = ref 0 in
      let less = ref 0 in
      let rebase = ref (-1) in
      let sets = ref [] in
      let fully_merged = ref true in
      OcpList.iteri (fun i (commit, merge) ->
        if not merge then begin
          let branches = Sha1Map.find commit !all_commits in
          let branches =
            if List.mem master !branches then [] else
              !branches in

          match !sets with
          | [] ->
            if !fully_merged then begin
              match branches with
                _ :: _  -> fully_merged := false
              | [] -> ()
            end;
            sets := [ branches, ref 1, ref commit ]
          | (old_branches, counter, last_commit) :: _ ->
            if old_branches = branches then begin
              incr counter;
              last_commit := commit;
            end  else begin
              if !fully_merged then begin
                match branches with
                   _ :: _ -> fully_merged := false
                | [] -> ()
              end;

              sets := (branches, ref 1, ref commit) :: !sets
            end
        end;
        if Sha1Set.mem commit master_set then begin
          if not merge then incr common
        end else begin
          if !rebase < 0 then rebase := i;
          if not merge then incr more
        end
      ) (List.rev commits);
      if !rebase < 0 then rebase := total;
      List.iter (function (commit, merge) ->
        if not ( Sha1Set.mem commit set ) && not merge then incr less
      ) master_commits;
      Printf.printf "  %-50s %d commits (%s)\n"  name total
        (match commits with
         | (head, _) :: _ -> Sha1.to_hex head
         | [] -> assert false);
      if !fully_merged then begin
        Printf.printf "     Fully merged in %s (%d backward)\n%!" master !less
      end else begin
        Printf.printf "     rebase %5d, common %5d, less %5d, more %5d\n%!"
          !rebase !common !less !more;
        List.iter (fun (branches, counter, last_commit) ->
          Printf.printf "    %5d %s (%s)\n%!" !counter
            (match branches with
             | [] -> "*"
             | _ -> String.concat "," branches) (Sha1.to_hex !last_commit)
        ) !sets
      end
    end
  ) branches

type action =
    CheckObjects
  | NoAction
let action_arg = ref NoAction

let arg_list = Arg.align
  [
    "-v", Arg.Unit (fun () ->
      incr Git.verbose ), " increase verbosity";
    "-rank", Arg.Unit rank_contributors, " : rank contributors";
    "-cat-file", Arg.String cat_file, " <filename> : cat file content";
    "-check-objects", Arg.Unit (fun _ -> action_arg := CheckObjects),
    " : check that all files on command line are correct GIT objects";
    "-branches", Arg.String get_branches, "MASTER print commits of all branches";
  ]

let arg_anon file =
  match !action_arg with
      NoAction -> Arg.usage arg_list arg_usage; exit 2
    | CheckObjects ->
      Git.check_object (FileGen.of_string file)

let _ =
  if Array.length Sys.argv < 2 then begin
    Arg.usage arg_list arg_usage;
    exit 2
  end;
  let arg1 = Sys.argv.(1) in
  if arg1.[0] = '-' then
    Arg.parse arg_list arg_anon arg_usage
  else begin
    Sys.argv.(0) <- "git";
    MinUnix.execvp Sys.argv.(0) Sys.argv
  end
