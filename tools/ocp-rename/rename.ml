
module SimpleConfig = struct

  include SimpleConfig

  let find_local basename =
    let rec iter dir =
      let filename = File.add_basename dir basename in
      if File.exists filename then Some filename
      else
        let dirdir = File.dirname dir in
        if dirdir == dir then None
        else iter dirdir
    in
    let dir = File.getcwd () in
    iter dir

  let create_local_config basename =
    let file =
      match find_local basename with
      | Some file -> file
      | None -> File.of_string basename
    in
    SimpleConfig.create_config_file file

  end

open SimpleConfig.Op

let local_config_basename =  ".ocp-renamings"
let local_config_file = File.of_string local_config_basename
let config = SimpleConfig.create_config_file local_config_file

let remove_strings = SimpleConfig.create_option config
    [ "remove_strings" ] [] ~short_help:""
    (SimpleConfig.list_option SimpleConfig.string_option) []

let replace_strings = SimpleConfig.create_option config
    [ "replace_strings" ] [ "replace pairs (old_string,new_string)"
                          ] ~short_help:""
    (SimpleConfig.list_option
       (SimpleConfig.tuple2_option (SimpleConfig.string_option,
                                   SimpleConfig.string_option))) []

let _ =
  let subst = StringSubst.empty_subst () in
  let remove_string new_string old_string =
    StringSubst.add_to_subst subst old_string new_string
  in
  let not_fake = ref true in
  let forced = ref false in
  let arg1 = ref "" in
  let arg2 = ref "" in
  let arg_list = [
    "-k", Arg.Clear not_fake, " Only print what would be done";
    "-f", Arg.Set forced, "  Erase targets if necessary";
    "-e", Arg.String (fun s ->
        remove_strings =:= s :: !!remove_strings;
        remove_string "" s), "STRING Remove STRING from filenames";
    "-s", Arg.Tuple [Arg.String ( (:=) arg1);
                     Arg.String ( (:=) arg2);
                     Arg.Unit (fun () ->
                         remove_string !arg2 !arg1
                       ) ], "OLD NEW Replace OLD by NEW";
    "-save", Arg.Unit (fun () ->
        SimpleConfig.save config), " Save .ocp-renamings file";
  ] in

  let rename dirname basename =
    let (nocc, new_basename) = StringSubst.iter_subst subst basename in
    if new_basename <> basename then begin
      let old_filename = Filename.concat dirname basename in
      let new_filename = Filename.concat dirname new_basename in
      let do_it =
        if Sys.file_exists new_filename then begin
          Printf.eprintf
            "Warning: target already exists:\n  old:%s\n  new:%s\n%!"
            old_filename new_filename;
          !forced
        end else true
      in
      if do_it then begin
        Printf.eprintf "mv %s %s\n%!" old_filename new_filename;
        if !not_fake then begin
          Sys.rename old_filename new_filename
        end
      end
    end
  in
  let need_init = ref true in
  let read_config () =
    need_init := false;
    match SimpleConfig.find_local local_config_basename with
    | None -> ()
    | Some file ->
      SimpleConfig.set_config_file config file;
      SimpleConfig.load config;
      List.iter (remove_string "") !!remove_strings;
      List.iter (fun (old_s, new_s) ->
          remove_string new_s old_s) !!replace_strings;
  in
  let rec arg_anon dirname =
    if !need_init then read_config ();
    let files = Sys.readdir dirname in
    Array.iter (fun basename ->
        let filename = Filename.concat dirname basename in
        if Sys.is_directory filename then arg_anon filename
        else
          rename dirname basename
      ) files
  in
  let arg_usage = "ocp-rename [OPTIONS] DIRS" in
  Arg.parse arg_list arg_anon arg_usage
