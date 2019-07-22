type attributes

val deps : attributes -> string list
  
val make_attributes : (Temperature.t , string) Resultx.t ->
                      (State.t , string) Resultx.t ->
                      (string list, string) Resultx.t ->
                      (attributes, string) Resultx.t

module V : Graph.Sig.VERTEX with type t = string * attributes

val state : V.t -> State.t

val temperature : V.t -> Temperature.t

val name : V.t -> string


