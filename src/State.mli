type t =
  | Done
  | Blocked
  | Ready
  | Next

val read : (string, string) Resultx.t -> (t, string) Resultx.t

val compare : t -> t -> int

val to_enum : t -> int

val of_enum : int -> t option
