(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

(*

  $ ocp-watch build COMMAND ARGS

  will return the result of calling COMMAND with ARGS. ocp-watch
  might not call the command if it is not necessary.

  $ ocp-watch record COMMAND ARGS

  All files should be saved into a directory of files .ocp-watch/objects/.

*)

open WatchCommon
(*
  open Msgipc.TYPES
*)

let _ =
  Printexc.register_printer (fun exn ->
    match exn with
      Unix.Unix_error (error, s1, s2) ->
        Some (Printf.sprintf "Unix_error(%s, %s, %s)"
                (Unix.error_message error) s1 s2)
    | _ -> None)
;;

let _ =
  Subcommands.parse []
    [
      "record", WatchRecord.subcmd_init, WatchRecord.subcmd_spec, WatchRecord.subcmd_main;
      "report", WatchReport.subcmd_init, WatchReport.subcmd_spec, WatchReport.subcmd_main;
      "install", WatchInstall.subcmd_init, WatchInstall.subcmd_spec, WatchInstall.subcmd_main;
    ]
    (String.concat "\n"
       ["ocp-watch WATCH-COMMAND [OPTIONS] ARGUMENTS";
        "where WATCH-COMMAND:";
        ""
       ]
    )
