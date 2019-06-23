(* abstract type of flow graph *)
type t

val of_json : Yojson.Basic.t -> (t, string) Resultx.t

val has_cycle : t -> bool

val check_consistency : t -> (unit, string) Resultx.t
