
val verbose : int ref

val read_git :
  File.t ->  (* The current repository to be read *)
  File.t ->  (* Some shared directory where unpacked files can be stored *)
  GitTypes.repo

val check_object : File.t -> unit
