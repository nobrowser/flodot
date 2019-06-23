type attributes

module V : Graph.Sig.VERTEX with type t = string * attributes

val state : V.t -> State.t

val temperature : V.t -> Temperature.t

val name : V.t -> string

val graph : Yojson.Basic.t -> (V.t list * (V.t * V.t) list, Yojson.Basic.t * string) result
