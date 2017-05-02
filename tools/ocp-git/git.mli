
val verbose : int ref

val read_git :
  FileGen.t ->  (* The current repository to be read *)
  FileGen.t ->  (* Some shared directory where unpacked files can be stored *)
  GitTypes.repo

val check_object : FileGen.t -> unit
