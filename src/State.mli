type t =
  | Done
  | Blocked
  | Ready
  | Next

val read : string -> (t, string) result

val compare : t -> t -> int

val to_enum : t -> int

val of_enum : int -> t option
