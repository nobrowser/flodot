type t =
  | Done
  | Blocked
  | Ready
  | Next

exception Invalid of string
  
val read : string -> t

val compare : t -> t -> int

val to_enum : t -> int

val of_enum : int -> t option
