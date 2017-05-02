exception BadMagic of string

val read : FileGen.t -> GitTypes.repo
val write : GitTypes.repo -> unit
