
type t

val mod_temp_required : bool -> t -> t

val set : t -> unit
val get : unit -> t
val get_temp_required : t -> bool
