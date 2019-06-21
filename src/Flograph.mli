open Sm

(* abstract type of flow graph *)
type t

val of_attributes : Unlinked.t StringMap.t -> t
