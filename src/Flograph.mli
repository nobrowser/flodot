open Sm

(* abstract type of flow graph *)
type t

val of_attributes : Unlinked.t StringMap.t -> t

val has_cycle : t -> bool

val check_consistency : Unlinked.t StringMap.t -> t -> (unit, string) result
