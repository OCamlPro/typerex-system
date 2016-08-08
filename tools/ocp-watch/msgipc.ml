(***********************************************************************)
(*                                                                     *)
(*                             ocp-watch                               *)
(*                                                                     *)
(*  Copyright 2012 OCamlPro SAS                                        *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open StringCompat

type t = int

module TYPES = struct

  type 'a msg = {
    mutable msg_id : int;
    mutable msg_size : int;
    msg_payload : 'a;
  }
end
type 'a msg = 'a TYPES.msg

external msgqueue_private_c : unit -> t = "msgqueue_private_ml"
external msgqueue_receive_c : t -> int -> bytes msg -> unit = "msgqueue_receive_ml"
external msgqueue_send_c : t -> string msg -> unit = "msgqueue_send_ml"
external msgqueue_delete_c : t -> unit = "msgqueue_delete_ml"
external msgqueue_sizeofid_c : unit -> int = "msgqueue_sizeofid_ml"

let create_private = msgqueue_private_c
let recv = msgqueue_receive_c
let send = msgqueue_send_c
let delete = msgqueue_delete_c

let to_int t = t
let of_int t = t

let size_of_id = msgqueue_sizeofid_c()
