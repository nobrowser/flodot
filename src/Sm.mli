module StringMap : Map.S with type key = string

val of_results : (string * ('a, string) Resultx.t) list ->
                 ('a StringMap.t, string) Resultx.t
