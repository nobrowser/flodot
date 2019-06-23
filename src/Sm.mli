module StringMap : Map.S with type key = string

exception Not_found_n of string

val find' : string -> 'a StringMap.t -> 'a
