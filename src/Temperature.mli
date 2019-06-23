type t

exception Invalid of string
   
val read : string -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val to_enum : t -> int

val of_enum : int -> t option
