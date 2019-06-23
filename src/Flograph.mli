(* abstract type of flow graph *)
type t

val of_json : Yojson.Basic.t -> (t, Yojson.Basic.t * string) result

val has_cycle : t -> bool

val check_consistency : t -> (unit, string) result
