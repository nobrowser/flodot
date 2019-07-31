type attributes

val find_deps_opt : f:(string -> bool) -> attributes -> string option

val fold_left_deps : f:('a -> string -> 'a) -> 'a -> attributes -> 'a

val make_attributes : (Temperature.t , string) result ->
                      (State.t , string) result ->
                      (string list, string) result ->
                      (attributes, string) result

module V : Graph.Sig.VERTEX with type t = string * attributes

val state : V.t -> State.t

val temperature : V.t -> Temperature.t

val name : V.t -> string
