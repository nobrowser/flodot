module StringMap : Map.S with type key = string

exception Ex of string
     
val check_dupes : (string * 'a) list ->'a StringMap.t
