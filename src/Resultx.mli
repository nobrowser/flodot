type ('a, 'e) t = ('a, 'e) result

val ok : 'a -> ('a, 'e) t

val error : 'e -> ('a, 'e) t

val value : ('a, 'e) t -> default:'a -> 'a

val get_ok : ('a, 'e) t -> 'a

val get_error : ('a, 'e) t -> 'e

val bind : ('a, 'e) t ->
           ('a -> ('b, 'e) t) -> ('b, 'e) t

val join : (('a, 'e) t, 'e) t -> ('a, 'e) t

val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) t -> 'c

val iter : ('a -> unit) -> ('a, 'e) t -> unit

val iter_error : ('e -> unit) -> ('a, 'e) t -> unit

val is_ok : ('a, 'e) t -> bool

val is_error : ('a, 'e) t -> bool

val equal : ok:('a -> 'a -> bool) ->
            error:('e -> 'e -> bool) ->
            ('a, 'e) t -> ('a, 'e) t -> bool

val compare : ok:('a -> 'a -> int) ->
              error:('e -> 'e -> int) ->
              ('a, 'e) t -> ('a, 'e) t -> int

val to_option : ('a, 'e) t -> 'a option

val to_list : ('a, 'e) t -> 'a list

val to_seq : ('a, 'e) t -> 'a Seq.t
