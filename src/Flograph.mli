(* abstract type of flow graph *)
type t

val of_json : temp_required:bool -> Yojson.Basic.t -> (t, string) Resultx.t

val has_cycle : t -> bool

val check_consistency : t -> (t, string) Resultx.t

val output_dot : Stdlib.out_channel -> t -> (unit, string) Resultx.t
