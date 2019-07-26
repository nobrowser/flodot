val tokens : f:(char -> bool) -> string -> string list

val fields : f:(char -> bool) -> string -> string list

val token_bounds : f:(char -> bool) -> string -> (int * int) list

val field_bounds : f:(char -> bool) -> string -> (int * int) list

val is_prefix : string -> string -> bool
