module StringMap : Map.S with type key = string

val check_dupes : (string * 'a) list -> ('a StringMap.t, string) Resultx.t

module StringMapRx : Resultx.MAPS
       with module M = StringMap
