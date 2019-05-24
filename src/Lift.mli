val (|>>) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result

val (|>>|) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result

val fold :
  ('a -> ('b, 'c) result) ->
  ('d -> 'b -> 'e) -> ('d, 'c) result -> 'a -> ('e, 'c) result
