type attributes

module V : Graph.Sig.VERTEX with type t = string * attributes

module E : Graph.Sig.EDGE with type t = V.t * V.t

val graph : Yojson.Basic.t -> (V.t list * E.t list, Yojson.Basic.t * string) result
