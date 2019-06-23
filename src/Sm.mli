module StringMap : Map.S with type key = string

val of_results : (string * ('a, string) FutureResult.t) list ->
                 ('a StringMap.t, string) FutureResult.t
