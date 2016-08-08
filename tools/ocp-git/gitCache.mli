exception BadMagic of string

val read : File.t -> GitTypes.repo
val write : GitTypes.repo -> unit
