open StringCompat

(*
GIT index format
================

= The git index file has the following format

  All binary numbers are in network byte order. Version 2 is described
  here unless stated otherwise.

   - A 12-byte header consisting of

   - A number of sorted index entries (see below).

   - Extensions

     Extensions are identified by signature. Optional extensions can
     be ignored if GIT does not understand them.

     GIT currently supports cached tree and resolve undo extensions.

     4-byte extension signature. If the first byte is 'A'..'Z' the
     extension is optional and can be ignored.

     32-bit size of the extension

     Extension data

   - 160-bit SHA-1 over the content of the index file before this
     checksum.

== Extensions

=== Cached tree

  Cached tree extension contains pre-computed hashes for trees that can
  be derived from the index. It helps speed up tree object generation
  from index for a new commit.

  When a path is updated in index, the path must be invalidated and
  removed from tree cache.

  The signature for this extension is { 'T', 'R', 'E', 'E' }.

  A series of entries fill the entire extension; each of which
  consists of:

  - NUL-terminated path component (relative to its parent directory);

  - ASCII decimal number of entries in the index that is covered by the
    tree this entry represents (entry_count);

  - A space (ASCII 32);

  - ASCII decimal number that represents the number of subtrees this
    tree has;

  - A newline (ASCII 10); and

  - 160-bit object name for the object that would result from writing
    this span of index as a tree.

  An entry can be in an invalidated state and is represented by having
  -1 in the entry_count field. In this case, there is no object name
  and the next entry starts immediately after the newline.

  The entries are written out in the top-down, depth-first order.  The
  first entry represents the root level of the repository, followed by the
  first subtree---let's call this A---of the root level (with its name
  relative to the root level), followed by the first subtree of A (with
  its name relative to A), ...

=== Resolve undo

  A conflict is represented in the index as a set of higher stage entries.
  When a conflict is resolved (e.g. with "git add path"), these higher
  stage entries will be removed and a stage-0 entry with proper resoluton
  is added.

  When these higher stage entries are removed, they are saved in the
  resolve undo extension, so that conflicts can be recreated (e.g. with
  "git checkout -m"), in case users want to redo a conflict resolution
  from scratch.

  The signature for this extension is { 'R', 'E', 'U', 'C' }.

  A series of entries fill the entire extension; each of which
  consists of:

  - NUL-terminated pathname the entry describes (relative to the root of
    the repository, i.e. full pathname);

  - Three NUL-terminated ASCII octal numbers, entry mode of entries in
    stage 1 to 3 (a missing stage is represented by "0" in this field);
    and

  - At most three 160-bit object names of the entry in stages from 1 to 3
    (nothing is written for a missing stage).

      *)


(* Copied from ocp-git, git.ml file. Should be replaced by ocplib-endian *)
module Binary = struct
  let get_uint8 s pos =
    int_of_char s.[pos]

  let get_int16 s pos =
    let c1 = int_of_char s.[pos] in
    let c2 = int_of_char s.[pos+1] in
    c2 lor (c1 lsl 8), pos + 2

  let get_int24 s pos =
    let c1 = int_of_char s.[pos] in
    let c2 = int_of_char s.[pos+1] in
    let c3 = int_of_char s.[pos+2] in
    c3 lor (c2 lsl 8) lor (c1 lsl 16), pos + 3

  let get_int31 s pos =
    let c1 = get_uint8 s pos in
    let c2 = get_uint8 s (pos+1) in
    let c3 = get_uint8 s (pos+2) in
    let c4 = get_uint8 s (pos+3) in
    let x =   c4 lor (c3 lsl 8) lor (c2 lsl 16) lor (c1 lsl 24) in
    x, pos + 4

  let get_int64_32 s pos =
    let c1 = get_uint8 s pos in
    let c2 = get_uint8 s (pos+1) in
    let c3 = get_uint8 s (pos+2) in
    let c4 = get_uint8 s (pos+3) in
    let x = Int64.logor
      (Int64.logor (Int64.of_int c4)
         (Int64.shift_left  (Int64.of_int c3) 8))
      (Int64.logor (Int64.shift_left  (Int64.of_int c2) 16)
         (Int64.shift_left  (Int64.of_int c1) 24)) in
    x, pos + 4

  let get_int64 s pos =
    let n3, pos = get_int16 s pos in
    let n2, pos = get_int16 s pos in
    let n1, pos = get_int16 s pos in
    let n0, pos = get_int16 s pos in
    Int64.logor
      (Int64.logor
         (Int64.shift_left (Int64.of_int n3) 48)
         (Int64.shift_left (Int64.of_int n2) 32))
      (Int64.logor
         (Int64.shift_left (Int64.of_int n1) 16)
         (Int64.of_int n0)),
    pos


end

type entry = {
  ctime_sec : int64;
  ctime_nanosec : int64;
  mtime_sec : int64;
  mtime_nanosec : int64;
  dev : int64;
  ino : int64;
  mode : int64;
  uid : int64;
  gid : int64;
  file_size : int64;
  name : string;
}

type index = {
  version : int;
  entries : entry array;
}



let of_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let s =
    let s = Bytes.create len in
    really_input ic s 0 len;
    Bytes.to_string s
  in
  assert (String.sub s 0 4 = "DIRC");
  let version, pos = Binary.get_int31 s 4 in
  assert (version = 2 || version = 3);
  let nentries, pos = Binary.get_int31 s pos in
(*  Printf.printf "version=%d, nentries = %d\n%!" version nentries; *)

  let pos = 12 in (* skip the header now *)
  let rec read_entries pos nentries entries =
    if nentries = 0 then entries else

      let ctime_sec, pos = Binary.get_int64_32 s pos in
(*  32-bit ctime seconds, the last time a file's metadata changed
    this is stat(2) data *)
      let ctime_nanosec, pos = Binary.get_int64_32 s pos in
(*  32-bit ctime nanosecond fractions
    this is stat(2) data *)
      let mtime_sec, pos = Binary.get_int64_32 s pos in
(*
  32-bit mtime seconds, the last time a file's data changed
    this is stat(2) data
*)
      let mtime_nanosec, pos = Binary.get_int64_32 s pos in
      (*
  32-bit mtime nanosecond fractions
    this is stat(2) data
*)
      let dev, pos = Binary.get_int64_32 s pos in
(*
  32-bit dev
    this is stat(2) data
*)
      let ino, pos = Binary.get_int64_32 s pos in
(*
  32-bit ino
    this is stat(2) data
*)
      let mode, pos = Binary.get_int64_32 s pos in
(*
  32-bit mode, split into (high to low bits)

    4-bit object type
      valid values in binary are 1000 (regular file), 1010 (symbolic link)
      and 1110 (gitlink)

    3-bit unused

    9-bit unix permission. Only 0755 and 0644 are valid for regular files.
    Symbolic links and gitlinks have value 0 in this field.
*)
      let uid, pos = Binary.get_int64_32 s pos in
(*      Printf.printf "uid=%Ld (pod=%d)\n%!" uid pos; *)
(*
  32-bit uid
    this is stat(2) data
*)
      let gid, pos = Binary.get_int64_32 s pos in
(*      Printf.printf "gid=%Ld (pos=%d)\n%!" uid pos; *)
(*
  32-bit gid
    this is stat(2) data
*)
      let file_size, pos = Binary.get_int64_32 s pos in
(*
  32-bit file size
    This is the on-disk size from stat(2), truncated to 32-bit.
*)
      let sha1, pos = String.sub s pos 20, pos+20 in

(*
  160-bit SHA-1 for the represented object
*)
      let flags, pos = Binary.get_int16 s pos in
      let name_length = flags land 0xFFF in
(*      Printf.printf "name_length = %d\n%!" name_length; *)
(*
  A 16-bit 'flags' field split into (high to low bits)

    1-bit assume-valid flag

    1-bit extended flag (must be zero in version 2)

    2-bit stage (during merge)

    12-bit name length if the length is less than 0xFFF; otherwise 0xFFF
    is stored in this field.
*)
      let extended, pos =
        if version = 3 then
          Binary.get_int16 s pos
        else
          0, pos
      in
(*
  (Version 3) A 16-bit field, only applicable if the "extended flag"
  above is 1, split into (high to low bits).

    1-bit reserved for future

    1-bit skip-worktree flag (used by sparse checkout)

    1-bit intent-to-add flag (used by "git add -N")

    13-bit unused, must be zero
*)
      let endpos = String.index_from s pos '\000' in
      let name = String.sub s pos (endpos - pos) in
      assert (String.length name = name_length || name_length = 0xfff);
      let pos = ((endpos + 8 - 12) / 8 * 8) + 12 in
(*
  Entry path name (variable length) relative to top level directory
    (without leading slash). '/' is used as path separator. The special
    path components ".", ".." and ".git" (without quotes) are disallowed.
    Trailing slash is also disallowed.

    The exact encoding is undefined, but the '.' and '/' characters
    are encoded in 7-bit ASCII and the encoding cannot contain a NUL
    byte (iow, this is a UNIX pathname).

  1-8 nul bytes as necessary to pad the entry to a multiple of eight bytes
  while keeping the name NUL-terminated.
*)
      let entry = {
        ctime_sec; ctime_nanosec;
        mtime_sec; mtime_nanosec;
        dev; ino; mode;
        uid; gid;
        file_size; name } in
      read_entries pos (nentries-1) (entry :: entries)
  in
  let entries = read_entries pos nentries [] in
  { version; entries = Array.of_list (List.rev entries) }
