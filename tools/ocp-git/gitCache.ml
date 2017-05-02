(**************************************************************************)
(*                                                                        *)
(*                             ocp-git                                    *)
(*                                                                        *)
(*  Copyright 2012-2015, OCamlPro                                         *)
(*                                                                        *)
(*  All rights reserved.  See accompanying files for the terms under      *)
(*  which this file is distributed. In doubt, contact us at               *)
(*  contact@ocamlpro.com (http://www.ocamlpro.com/)                       *)
(*                                                                        *)
(**************************************************************************)

open StringCompat
open GitTypes

exception BadMagic of string

let magic_length = 20
let magic_v1 = "OCPGIT-V001-20110919"

let read_magic ic =
  let magic = Bytes.create 20 in
  really_input ic magic 0 20;
  Bytes.to_string magic

let _ =
  assert (String.length magic_v1 = 20)

let filename git_dir =
  FileGen.add_basenames git_dir ["ocp-git.cache"]

let read git_dir =
  let file = filename git_dir in
  if not (FileGen.exists file) then raise Not_found;
  let ic = FileGen.open_in file in
  try
    let magic = read_magic ic in
    if magic <> magic_v1 then raise (BadMagic magic);
    let t = input_value ic in
    close_in ic;
    t
  with
    | BadMagic _ -> raise Not_found
    | e -> close_in ic; raise e

let write t =
  if t.git_updated then
    let oc = FileGen.open_out (filename t.git_dir) in
    output_string oc magic_v1;
    t.git_updated <- false;
    output_value oc t;
    close_out oc
