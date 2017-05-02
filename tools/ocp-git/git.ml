open StringCompat
open GitTypes

let dummy_object = {
  obj_sha1 = Sha1.null;
  obj_kind = MissingObject;
}

let verbose = ref 0

let string_of_KIND kind =
  match kind with
  | OBJ_COMMIT -> "OBJ_COMMIT"
  | OBJ_TREE -> "OBJ_TREE"
  | OBJ_BLOB -> "OBJ_BLOB"
  | OBJ_TAG -> "OBJ_TAG"
  | OBJ_OFS_DELTA obj ->
    Printf.sprintf "OBJ_OFS_DELTA (%d)" obj.packobj_pos
  | OBJ_REF_DELTA sha1 ->
    Printf.sprintf "OBJ_REF_DELTA (%s)" (Sha1.to_hex sha1)

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



let read_pack_idx file =
(*  Printf.fprintf stderr "file = %s\n%!" (FileGen.to_string file); *)
  let s = FileGen.read_file file in
  let _len = String.length s in
(*  Printf.fprintf stderr "%d %d %d %d\n%!"
    (int_of_char s.[0])
    (int_of_char s.[1])
    (int_of_char s.[2])
    (int_of_char s.[3]); *)
  if s.[0] = '\255' && s.[1] = 't' && s.[2] = 'O' && s.[3] = 'c' then
    let (version, _) = Binary.get_int31 s 4 in
    let (size, _) = Binary.get_int31 s (8 + 255 * 4) in
(*    Printf.fprintf stderr "version %d, size = %d\n%!" version size; *)
(*    Printf.fprintf stderr "len = %d, total = 8 + 1024 + 28 * size + 40 = %d\n%!" len (8 + 1024 + 28 * size + 40); *)

    let sha1_pos = 8 + 1024 in
    let sha1_array = Array.init size (fun i ->
      Sha1.direct_of_string (String.sub s (sha1_pos + i * 20) 20)
    ) in

    let crc_pos = sha1_pos + size * 20 in
    let crc_array = Array.init size (fun i ->
      let (crc, _) = Binary.get_int64_32 s (crc_pos + i * 4) in
      crc) in

    let offsets_pos = crc_pos + size * 4 in
    let big_offsets_pos = offsets_pos + size * 4 in
    let n = ref 0 in
    let offsets_array = Array.init size (fun i ->
      let offset_pos = offsets_pos + i * 4 in
      let x = int_of_char s.[offset_pos] in
      if x land 128 = 0 then
        let (offset, _) = Binary.get_int64_32 s offset_pos in
        offset
      else
        let (offset, _) = Binary.get_int64 s (big_offsets_pos + !n * 8) in
        incr n;
        offset
    ) in
    let trailer_pos = big_offsets_pos + !n * 8 in
    let packfile_sha1 = Sha1.direct_of_string (String.sub s trailer_pos 20) in
    let idxfile_sha1 = Sha1.direct_of_string (String.sub s (trailer_pos+20) 20) in
    {
      idx_filename = file;
      idx_version = version;
      idx_size = size;
      idx_sha1s = sha1_array;
      idx_offsets = offsets_array;
      idx_crcs = Some crc_array;
      idx_idx_sha1 = idxfile_sha1;
      idx_pack_sha1 = packfile_sha1;
    }

  else

  let (size, _) = Binary.get_int31 s (254 * 4) in
(*  Printf.fprintf stderr "version 1, size = %d\n%!" size; *)
(*  Printf.eprintf "len = %d, comp = 8 + 1024 + %d * 24 + 20 = %d\n%!"
    len size (1024 + size * 24 + 20); *)
  let offsets_array = Array.make size 0L in
  let sha1_array = Array.init size (fun i ->
    let (offset, pos) = Binary.get_int64_32 s (1024 + i * 4) in
    offsets_array.(i) <- offset;
    let sha1 = Sha1.direct_of_string (String.sub s pos 20) in
    sha1) in

  let trailer_pos = 1024 + size * 20 in
  let packfile_sha1 = Sha1.direct_of_string (String.sub s trailer_pos 20) in
  let idxfile_sha1 = Sha1.direct_of_string (String.sub s (trailer_pos+20) 20) in
    {
      idx_filename = file;
      idx_version = 1;
      idx_size = size;
      idx_sha1s = sha1_array;
      idx_offsets = offsets_array;
      idx_crcs = None;
      idx_idx_sha1 = idxfile_sha1;
      idx_pack_sha1 = packfile_sha1;
    }



let rec split_lines lines content pos len =
  try
    let pos2 = String.index_from content pos '\n' in
    if pos2 = pos then
      List.rev lines, String.sub content (pos+1) (len - pos - 1)
    else
      split_lines (String.sub content pos (pos2 - pos) :: lines) content (pos2+1) len
  with Not_found ->
    List.rev lines, String.sub content pos (len-pos)

let sha1_basenames sha1 =
  let filename = Sha1.to_hex sha1 in
  let dir = String.sub filename 0 2 in
  let name = String.sub filename 2 (Sha1.length * 2 - 2) in
  [ dir; name ]

let find_object_file t sha1 =
  let basenames = sha1_basenames sha1 in
  let filename = FileGen.add_basenames t.git_dir
    ("objects" :: basenames) in
  if FileGen.exists filename then
    Some filename
  else
    let filename = FileGen.add_basenames t.git_shared_dir
      ( "objects" :: basenames ) in
    if FileGen.exists filename then
      Some filename
    else
      None

let find_name t name =
  try
    StringMap.find name t.git_names
  with Not_found ->
    let git_name = {
      git_name = name;
    } in
    t.git_updated <- true;
    t.git_names <- StringMap.add name git_name t.git_names;
    git_name

let check_object file =
  Printf.fprintf stderr "Checking file %s\n%!" (FileGen.to_string file);
  let sha1 = FileGen.basename file in
  let s = FileGen.read_file file in
  let s = Zlib.uncompress_string s in
  let digest = Sha1.to_hex (Sha1.string s) in
  if String.sub digest 2 38 <> sha1 then begin
    Printf.fprintf stderr "Error with object %s: uncompressed digest %s\n"
     (FileGen.to_string file)  digest;
    Printf.fprintf stderr "Content (%d): [%s]\n%!" (String.length s) s;
    exit 2
  end


let sha1_of_hex obj s =
  try
    Sha1.of_hex s
  with e ->
    Printf.fprintf stderr "Error while reading object %s\n%!" (Sha1.to_hex obj.obj_sha1);
    raise e

let rec read_object t sha1 =
  try
    Hashtbl.find t.git_objects sha1
  with Not_found ->
    t.git_updated <- true;
    let obj =  {
      obj_sha1 = sha1;
      obj_kind  = MissingObject;
    } in
    Hashtbl.add t.git_objects sha1 obj;

    let filename = find_object_file t sha1 in
    let kind =
      match filename with
        Some filename ->
        if !verbose > 0 then begin
          if !verbose > 1 then
            Printf.eprintf "Reading object %s...%!" (FileGen.to_string filename)
          else
          Printf.eprintf ".%!";
        end;
        let s0 = FileGen.read_file filename in
        let s = Zlib.uncompress_string s0 in
        let digest2 = Sha1.string s in
        assert (digest2 = sha1);
        (*            let s1 = Zlib.uncompress_string (Zlib.compress_string s) in
                      assert (s1 = s); *)

        let (header, content) = OcpString.cut_at s '\000' in
        let (kind, size) = OcpString.cut_at header ' ' in
        let size = int_of_string size in
        assert (size = String.length content);
        begin
          match kind with
            "commit" ->
            let (headers, content) = split_lines [] content 0 size in
              (*
                List.iter (fun h ->
                Printf.fprintf stderr "header[%s]\n%!" h;) headers;
                Printf.fprintf stderr "content[%s]\n%!" content;
              *)
                let c = {
                  commit_message = content;
                  commit_tree = dummy_object;
                  commit_parents = [];
                  commit_author = find_name t "";
                  commit_committer = find_name t "";
                } in
                List.iter (fun header ->
                  let (o,v) = OcpString.cut_at header ' ' in
                  match o with
                      "tree" -> c.commit_tree <- read_object t (sha1_of_hex obj v)
                    | "parent" -> c.commit_parents <- read_object t (sha1_of_hex obj v) :: c.commit_parents
                    | "author" -> c.commit_author <- find_name t v
                    | "committer" -> c.commit_committer <- find_name t v
                    | _ ->
(*                      Printf.fprintf stderr "[commit] Discarding field [%s]\n%!" o *)
()
                ) headers;
                Commit c
            | "tree" ->
              let entries = parse_entries t content 0 (String.length content) [] in
              Tree {
                tree_entries = entries;
              }
            | "blob" ->
              Blob
            | _ ->
(*              Printf.fprintf stderr "kind = [%s]\n%!" kind; *)
            (*            Printf.fprintf stderr "BEGIN\n%s\nEND\n%!" content; *)
            (*            Printf.eprintf "Uncompressed: [%s]\n%!"
                          (String.escaped s); *)
              UnknownObject kind
end
        | None ->
          Printf.fprintf stderr "Object %s is missing\n%!"
            (Sha1.to_hex obj.obj_sha1);
          MissingObject
    in
    if !verbose > 1 then
      Printf.eprintf "done\n%!";
    obj.obj_kind <- kind;
    obj

  and parse_entries t content pos len entries =
    if pos = len then entries else
      let pos2 = String.index_from content pos ' ' in
      let pos3 = String.index_from content (pos2+1) '\000' in
      let entry_perm = String.sub content pos (pos2 - pos) in
      let entry_name = String.sub content (pos2+1) (pos3-pos2-1) in
      let entry_sha1 = Sha1.direct_of_string (String.sub content (pos3+1) 20) in
      let e = {
        entry_perm = entry_perm;
        entry_name = entry_name;
        entry_sha1 = read_object t entry_sha1;
      } in
      parse_entries t content (pos3+21) len (e :: entries)


let read_pack_pack t file idx =
(*  Printf.fprintf stderr "file = %s\n%!" (FileGen.to_string file); *)
  let s = FileGen.read_file file in
  let len = String.length s in
  assert (s.[0] = 'P' &&
      s.[1] = 'A' && s.[2] = 'C' && s.[3] = 'K');
  let version, pos = Binary.get_int31 s 4 in
  let entries, pos = Binary.get_int31 s pos in
  assert (entries = Array.length idx.idx_sha1s);

  let offset_table = Hashtbl.create 17 in
  let offsets = Array.init entries (fun i ->
    let pos = idx.idx_offsets.(i) in
    (pos, i)
  ) in
  Array.sort compare offsets;
  let packfile =  {
    pack_filename = file;
    pack_idx = idx;
    pack_content = s;
    pack_version = version;
    pack_objects = [||];
    pack_git = t;
  }
  in
(*  Printf.fprintf stderr "For %d objects: \n%!" entries; *)
  let objects = Array.init entries (fun i ->
(*    Printf.fprintf stderr "Object %d\n%!" i; *)
    let (obj_pos, obj_num) = offsets.(i) in
    let obj_pos = Int64.to_int obj_pos in
(*    Printf.fprintf stderr "current pos = %d\n%!" obj_pos; *)
    let c1 = int_of_char s.[obj_pos] in

    let rec iter s size c1 offset nbits =
      (*      Printf.fprintf stderr "char = %d\n%!" c1; *)
      if c1 land 0x80 = 0 then (size, offset) else
        let c1 = int_of_char s.[offset] in
        let c1L = Int64.of_int (c1 land 0x7f) in
        let size = Int64.logor size
          (Int64.shift_left c1L nbits) in
        iter s size c1 (offset+1) (nbits+7)
    in
    let kind = (c1 lsr 4) land 0x7 in
    let size, data_pos = iter s (Int64.of_int (c1 land 0xf)) c1 (obj_pos+1) 4 in
    let kind, data_pos = match kind with
      | 0b001 -> OBJ_COMMIT, data_pos
      | 0b010 -> OBJ_TREE, data_pos
      | 0b011 -> OBJ_BLOB, data_pos
      | 0b100 -> OBJ_TAG, data_pos
      | 0b110 ->
        let rec iter s data_pos offset =
          let c = int_of_char s.[data_pos] in
          let data_pos = data_pos + 1 in
          let offset = (offset lsl 7) lor (c land 0x7f) in
          if c land 0x80 = 0 then (offset, data_pos)
          else iter s data_pos (offset+1)
        in
        let (offset, data_pos) = iter s data_pos 0 in
        OBJ_OFS_DELTA (Hashtbl.find offset_table (obj_pos - offset)), data_pos
      | 0b111 ->
        let sha1 = Sha1.direct_of_string (String.sub s data_pos 20) in
        OBJ_REF_DELTA sha1, data_pos + 20
      | _ -> assert false
    in
    let next_object_pos =
      if i + 1 < entries then
        Int64.to_int (fst offsets.(i+1))
      else
        len - 20
    in
    let pack_compressed_size = next_object_pos - data_pos in
(*    Printf.fprintf stderr "at pos %d -> %d\n%!" obj_pos data_pos; *)
(*    Printf.fprintf stderr "\tobject %s of size %Ld (compressed %d)\n%!"
      (string_of_KIND kind)
      size pack_compressed_size; *)
(* TODO: add a flag to do this kind of tests
    let content = Zlib.uncompress_string
      (String.sub s data_pos pack_compressed_size) in
    assert (String.length content = Int64.to_int size); *)
(*    Printf.fprintf stderr "CONTENT: [%s]\n%!" content; *)
    let obj = {
      packobj_pos = obj_pos;
      packobj_num = obj_num;
      packobj_sha1 = idx.idx_sha1s.(obj_num);
      packobj_kind = kind;
      packobj_data_pos = data_pos;
      packobj_expanded_size = size;
      packobj_compressed_size = pack_compressed_size;
      packobj_file = packfile;
      packobj_packed = true;
    } in
    Hashtbl.add offset_table  obj_pos obj;
    obj
  ) in
  packfile.pack_objects <- objects;
(*  Printf.fprintf stderr "Done with packfile\n%!"; *)
  packfile

(*


/*
 * This must be called twice on the delta data buffer, first to get the
 * expected source buffer size, and again to get the target buffer size.
 */
static inline unsigned long get_delta_hdr_size(const unsigned char **datap,
					       const unsigned char *top)
{
	const unsigned char *data = *datap;
	unsigned char cmd;
	unsigned long size = 0;
	int i = 0;
	do {
		cmd = *data++;
		size |= (cmd & ~0x80) << i;
		i += 7;
	} while (cmd & 0x80 && data < top);
	*datap = data;
	return size;
}

void *patch_delta(const void *src_buf, unsigned long src_size,
		  const void *delta_buf, unsigned long delta_size,
		  unsigned long *dst_size)
{
	const unsigned char *data, *top;
	unsigned char *dst_buf, *out, cmd;
	unsigned long size;

	if (delta_size < DELTA_SIZE_MIN)
		return NULL;

	data = delta_buf;
	top = (const unsigned char * ) delta_buf + delta_size;

	/* make sure the orig file size matches what we expect */
	size = get_delta_hdr_size(&data, top);
	if (size != src_size)
		return NULL;

	/* now the result size */
	size = get_delta_hdr_size(&data, top);
	dst_buf = xmalloc(size + 1);
	dst_buf[size] = 0;

	out = dst_buf;
	while (data < top) {
		cmd = *data++;
		if (cmd & 0x80) {
			unsigned long cp_off = 0, cp_size = 0;
			if (cmd & 0x01) cp_off = *data++;
			if (cmd & 0x02) cp_off |= ( *data++ << 8);
			if (cmd & 0x04) cp_off |= ( *data++ << 16);
			if (cmd & 0x08) cp_off |= ( *data++ << 24);
			if (cmd & 0x10) cp_size = *data++;
			if (cmd & 0x20) cp_size |= ( *data++ << 8);
			if (cmd & 0x40) cp_size |= ( *data++ << 16);
			if (cp_size == 0) cp_size = 0x10000;
			if (cp_off + cp_size < cp_size ||
			    cp_off + cp_size > src_size ||
			    cp_size > size)
				break;
			memcpy(out, (char * ) src_buf + cp_off, cp_size);
			out += cp_size;
			size -= cp_size;
		} else if (cmd) {
			if (cmd > size)
				break;
			memcpy(out, data, cmd);
			out += cmd;
			data += cmd;
			size -= cmd;
		} else {
			/*
			 * cmd == 0 is reserved for future encoding
			 * extensions. In the mean time we must fail when
			 * encountering them (might be data corruption).
			 */
			error("unexpected delta opcode 0");
			goto bad;
		}
	}

	/* sanity check */
	if (data != top || size != 0) {
		error("delta replay has gone wild");
		bad:
		free(dst_buf);
		return NULL;
	}

	*dst_size = out - dst_buf;
	return dst_buf;
}

*)

let read_branch t name remote file =
  if !verbose > 0 then
    Printf.eprintf "branch to read %s:%!" (FileGen.to_string file);
  let s = FileGen.read_file file in
(*  Printf.eprintf "content: %s\n%!" s; *)
  if OcpString.starts_with s "ref:" then None else
  let sha1 = Sha1.of_hex s in
(*  Printf.fprintf stderr "read_branch %s\n%!" (FileGen.to_string file); *)
  let o = read_object t sha1 in
  if !verbose > 0 then
    Printf.eprintf "done with branch %s\n%!" (FileGen.to_string file);
  Some {
    branch_name = name;
    branch_object = o;
    branch_remote = remote;
    branch_tracking = None;
  }

let read_from_pack filename pos size =
(*  Printf.fprintf stderr "read_from_pack...\n%!"; *)
  let s = FileGen.read_subfile filename pos size in
  let s = Zlib.uncompress_string s in
(*  Printf.fprintf stderr "read_from_pack done\n%!"; *)
  s

let save_object dir sha1 kind content =
  Printf.fprintf stderr "save_object %s\n%!" (Sha1.to_hex sha1);
  let b = Buffer.create 1111 in
  Printf.bprintf b "%s %d\000%s" kind (String.length content) content;
  let s = Buffer.contents b in
  let digest = Sha1.string s in
  let s = Zlib.compress_string s in
  assert (digest = sha1);
  let basenames = sha1_basenames sha1 in
  let filename = FileGen.add_basenames dir
    ( "objects" :: basenames ) in
  let dirname = FileGen.dirname filename in
  FileDir.make_all dirname;
  FileGen.write_file filename s

let gen_from_delta git_dir sha1 delta filename =
  Printf.fprintf stderr "gen_from_delta %s...\n%!" (Sha1.to_hex sha1);
  let source = FileGen.read_file filename in
  let source = Zlib.uncompress_string source in
  let (header, source) = OcpString.cut_at source '\000' in
  let (kind, source_size) = OcpString.cut_at header ' ' in
  let source_size = int_of_string source_size in
  assert (source_size = String.length source);
(*  Printf.fprintf stderr "SOURCE (%d):\n%s\n" (String.length source) source; *)
(*  Printf.fprintf stderr "DELTA (%d):\n%s\n" (String.length delta)
    (String.escaped delta); *)
  let b = Buffer.create (String.length delta + String.length source) in

  let len = String.length delta in

  let rec get_size delta pos size nbits =
    let cmd = int_of_char delta.[pos] in
    let pos = pos + 1 in
    let cmdsize = cmd land 0x7f in
    let size = size lor (cmdsize lsl nbits) in
    let nbits = nbits + 7 in
    if cmd land 0x80 <> 0 then
      get_size delta pos size nbits
    else (size, pos)
  in
  let (orig_size, pos) = get_size delta 0 0 0 in
(*  Printf.fprintf stderr "orig_size = %d\n%!" orig_size; *)
  assert (String.length source = orig_size);
  let (dest_size, pos) = get_size delta pos 0 0 in

  let rec iter delta pos len =
    if pos < len then
      let cmd = int_of_char delta.[pos] in
(*      Printf.fprintf stderr "cmd[%d]= %x\n%!" pos cmd; *)
      let pos = pos + 1 in
      if cmd land 0x80 <> 0 then
        let pos = ref pos in
        let cp_off = ref 0 in
        let cp_size = ref 0 in
        if cmd land 0x01 <> 0 then begin
          cp_off := int_of_char delta.[!pos]; incr pos;
        end;
        if cmd land 0x02 <> 0 then begin
          cp_off := !cp_off lor (int_of_char delta.[!pos] lsl 8); incr pos;
        end;
        if cmd land 0x04 <> 0 then begin
          cp_off := !cp_off lor (int_of_char delta.[!pos] lsl 16); incr pos;
        end;
        if cmd land 0x08 <> 0 then begin
          cp_off := !cp_off lor (int_of_char delta.[!pos] lsl 24); incr pos;
        end;
        if cmd land 0x10 <> 0 then begin
          cp_size := int_of_char delta.[!pos]; incr pos;
        end;
        if cmd land 0x20 <> 0 then begin
          cp_size := !cp_size lor (int_of_char delta.[!pos] lsl 8); incr pos;
        end;
        if cmd land 0x40 <> 0 then begin
          cp_size := !cp_size lor (int_of_char delta.[!pos] lsl 16); incr pos;
        end;
        if !cp_size = 0 then cp_size := 0x10000;
(*        Printf.fprintf stderr "copy from source pos=%d size=%d len=%d\n%!"
          !cp_off !cp_size (String.length source); *)
        Buffer.add_substring b source !cp_off !cp_size;
        iter delta !pos len
      else begin
(*        Printf.fprintf stderr "copy from delta pos=%d size=%d len=%d\n%!"
          pos cmd (String.length delta); *)
        Buffer.add_substring b delta pos cmd;
        iter delta (pos+cmd) len
      end

  in
  iter delta pos len;
  let s = Buffer.contents b in
(*  Printf.fprintf stderr "RESULT (%d):\n%s\n" (String.length s) s; *)
  assert (dest_size = String.length s);
  let s = Printf.sprintf "%s %d\000%s" kind dest_size s in
  let digest = Sha1.string s in
  assert (digest = sha1);
  let s = Zlib.compress_string s in
  let basenames = sha1_basenames sha1 in
  let filename = FileGen.add_basenames git_dir
    ( "objects" :: basenames ) in
  let dirname = FileGen.dirname filename in
  FileDir.make_all dirname;
  FileGen.write_file filename s




let read_git git_dir git_shared_dir =
  if !verbose > 0 then
    Printf.fprintf stderr "Found git dir in %s\n%!" (FileGen.to_string git_dir);

  let _config = PythonConfig.read (FileGen.add_basename git_dir "config") in
  if !verbose > 0 then
    Printf.eprintf "Read config file\n%!";

  let t = try
            GitCache.read git_dir
    with Not_found ->
      {
        git_shared_dir = git_shared_dir;
        git_dir = git_dir;
        git_heads = StringMap.empty;
        git_remotes = StringMap.empty;
        git_objects = Hashtbl.create 1111;
        git_packs = StringMap.empty;
        git_updated = true;
        git_names = StringMap.empty;
      }
  in

  (* start with pack files *)
  let pack_dir = FileGen.add_basenames git_dir ["objects"; "pack" ] in
  let sha1_table = Hashtbl.create 1111 in

  if !verbose > 0 then
    Printf.eprintf "Reading pack files...\n%!";
  FileDir.iter (fun name ->
    if Filename.check_suffix name ".idx" then
      let pack_name = Filename.chop_suffix name ".idx" in
      if not (StringMap.mem pack_name t.git_packs) then
        let idx_filename = FileGen.add_basename pack_dir name in
        let idx = read_pack_idx idx_filename in
        let pack_filename = FileGen.add_basename pack_dir
          (pack_name ^ ".pack") in
        let pack = read_pack_pack t pack_filename idx in
        t.git_packs <- StringMap.add pack_name pack t.git_packs;
        t.git_updated <- true;
        Array.iter (fun obj ->
          Hashtbl.add sha1_table obj.packobj_sha1 obj
        ) pack.pack_objects;
  ) pack_dir;

  if !verbose > 0 then
    Printf.fprintf stderr "Unpacking objects...\n%!";
  let rec unpack_object obj =
    if obj.packobj_packed then
      let sha1 = obj.packobj_sha1 in
      obj.packobj_packed <- false;
      (*      Printf.fprintf stderr "With %s\n%!" (Sha1.to_hex sha1); *)
      match find_object_file t sha1 with
          Some _ -> ()
        | None ->
          Printf.fprintf stderr "Unpacking %s\n%!" (Sha1.to_hex sha1);
          let content =
            read_from_pack
              obj.packobj_file.pack_filename
              obj.packobj_data_pos
              obj.packobj_compressed_size
          in
          match obj.packobj_kind with
            | OBJ_COMMIT
            | OBJ_TAG
            | OBJ_BLOB
            | OBJ_TREE ->
              let kind =
                match obj.packobj_kind with
                  | OBJ_COMMIT -> "commit"
                  | OBJ_TAG -> "tag"
                  | OBJ_BLOB -> "blob"
                  | OBJ_TREE -> "tree"
                  | _ -> assert false
              in
              save_object t.git_shared_dir obj.packobj_sha1 kind content
            | OBJ_REF_DELTA  delta_sha1 ->
              let rec iter retry =
                let file2 = find_object_file t delta_sha1 in
                begin
                  match file2 with
                      None ->
                        begin try
                                let obj2 =
                                  Hashtbl.find sha1_table delta_sha1 in
                                if obj2.packobj_packed && retry then begin
                                  unpack_object obj2;
                                  iter false
                                end else
                                  failwith "Unpacked unavailable"
                          with Not_found -> assert false (* TODO: error *)
                        end
                    | Some filename2 ->
                      gen_from_delta t.git_shared_dir obj.packobj_sha1
                        content filename2
                end
              in iter true
            | OBJ_OFS_DELTA  obj2 ->
              let rec iter retry =
                let file2 = find_object_file t obj2.packobj_sha1 in
                match file2 with
                    None ->
                      if retry then begin
                        unpack_object obj2;
                        iter false
                      end else
                        assert false
                  | Some filename2 ->
                    gen_from_delta t.git_shared_dir obj.packobj_sha1
                      content filename2
              in
              iter true
  in
  let rec expanse_pack_file _ pack =
    Array.iter unpack_object pack.pack_objects
  in
  StringMap.iter expanse_pack_file t.git_packs;

  if !verbose > 0 then
    Printf.fprintf stderr "Pack files expansed\n%!";

  let refs_dir = FileGen.add_basename git_dir "refs" in
  let heads_dir = FileGen.add_basename refs_dir "heads" in
  FileDir.iter (fun name ->
    match read_branch t name None (FileGen.add_basename heads_dir name) with
        None -> assert false
      | Some b ->
          t.git_heads <- StringMap.add name b t.git_heads
  ) heads_dir;
  let remotes_dir = FileGen.add_basename refs_dir "remotes" in
  FileDir.iter (fun remote ->
    if !verbose > 0 then
      Printf.fprintf stderr "remote : %s\n%!" remote;
    let remote_dir = FileGen.add_basename remotes_dir remote in
    let r = { remote_name = remote; remote_branches = StringMap.empty } in
    t.git_remotes <- StringMap.add remote r t.git_remotes;
    FileDir.iter (fun name ->
      match  read_branch t name (Some r) (FileGen.add_basename remote_dir name) with
          None -> ()
        | Some b ->
          r.remote_branches <- StringMap.add name b r.remote_branches
    ) remote_dir;
  ) remotes_dir;

  GitCache.write t;

  t
