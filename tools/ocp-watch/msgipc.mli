
open StringCompat

type t

module TYPES : sig
type 'a msg = {
  mutable msg_id : int;
  mutable msg_size : int;
  msg_payload : 'a;
}

end

type 'a msg = 'a TYPES.msg

(* [create_private()] returns a private message queue *)
val create_private : unit -> t

(* [recv mq buf] waits and fills [buf] with the first message received
   on [mq] *)
val recv : t -> int -> bytes msg -> unit

(* [send mq buf len] sends the first [len] bytes of [buf] on [mq] as a
   single message, waiting if the queue is full. *)
val send : t -> string msg -> unit

(* [delete mq] destroys the message queue [mq] *)
val delete : t -> unit

val to_int : t -> int
val of_int : t -> int

val size_of_id : int
