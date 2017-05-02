
open OcpCompat

type repo = {
  git_dir : FileGen.t;
  git_shared_dir : FileGen.t;
  mutable git_heads : branch StringMap.t;
  mutable git_remotes : remote StringMap.t;
  mutable git_objects : (Sha1.t, git_object) Hashtbl.t;
  mutable git_packs : pack_file StringMap.t;
  mutable git_updated : bool;
  mutable git_names : git_name StringMap.t;
}

and git_name = {
  git_name : string;
}

and remote = {
  remote_name : string;
  mutable remote_branches : branch StringMap.t;
}

and branch = {
  branch_name : string;
  branch_remote : remote option;
  branch_object : git_object;
  mutable branch_tracking : branch option;
}

and git_object = {
  obj_sha1 : Sha1.t;
  mutable obj_kind : obj_kind;
}

and obj_kind =
  | MissingObject
  | UnknownObject of string

  | Commit of git_commit
  | Tree of git_tree
  | Blob

and obj_KIND =
  | OBJ_COMMIT
  | OBJ_TREE
  | OBJ_BLOB
  | OBJ_TAG
  | OBJ_OFS_DELTA of pack_object
  | OBJ_REF_DELTA of Sha1.t

and git_commit = {
  mutable commit_tree : git_object;
  mutable commit_parents : git_object list;
  mutable commit_author : git_name;
  mutable commit_committer : git_name;
  mutable commit_message : string;
}

and git_tree = {
  mutable tree_entries : tree_entry list;
}

and tree_entry = {
  entry_perm : string;
  entry_name : string;
  entry_sha1 : git_object;
}

and idx_file = {
  idx_filename : FileGen.t;
  idx_version : int;
  idx_size : int;
  idx_sha1s : Sha1.t array;
  idx_offsets : int64 array;
  idx_crcs : int64 array option;
  idx_idx_sha1 : Sha1.t;
  idx_pack_sha1 : Sha1.t;
}

and pack_file = {
  pack_git : repo;
  pack_filename : FileGen.t;
  pack_version : int;
  mutable pack_objects : pack_object array;
  pack_idx : idx_file;
  pack_content : string;
}

and pack_object =
    {
      packobj_file : pack_file;
      packobj_sha1 : Sha1.t;
      packobj_pos : int;
      packobj_num : int;
      packobj_kind : obj_KIND;
      packobj_data_pos : int;
      packobj_expanded_size : int64;
      mutable packobj_compressed_size : int;
      mutable packobj_packed : bool;
    }
