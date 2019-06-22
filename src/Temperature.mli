type t

val read : string -> (t, string) result

val equal : t -> t -> bool

val compare : t -> t -> int

val to_enum : t -> int

val of_enum : int -> t option
