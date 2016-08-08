(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

let filename = Sys.argv.(1)
let dirname = Filename.dirname filename

let copy_escaped oc filename =
  let filename = Filename.concat dirname filename in
  let ic = open_in filename in
  let nlines = ref 0 in
  Printf.fprintf oc "#line 1 \\\"%s\\\"\\n" filename;
  try
    while true do
      let line = input_line ic in
      incr nlines;
      output_string oc (String.escaped line);
      output_string oc "\\n";
    done; assert false
  with End_of_file ->
    close_in ic;
    !nlines

let _ =
  let oc = stdout in

  Printf.fprintf oc "let md5 = \"";
  let nlines = copy_escaped oc "md5.c" in
  Printf.fprintf oc "\"\n";
  Printf.fprintf oc "let md5_nlines = %d\n" nlines;

  Printf.fprintf oc "let header = \"";
  let nlines = copy_escaped oc "ocp_watcher.header.c" in
  Printf.fprintf oc "\"\n";
  Printf.fprintf oc "let header_nlines = %d\n" nlines;

  Printf.fprintf oc "let trailer = \"";
  let nlines = copy_escaped oc "ocp_watcher.trailer.c" in
  Printf.fprintf oc "\"\n";
  Printf.fprintf oc "let trailer_nlines = %d\n" nlines;

  close_out oc

