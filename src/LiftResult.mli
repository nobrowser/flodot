val (|>>) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result

val (|>>|) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result

val fold :
  ('a -> ('b, 'c) result) ->
  ('d -> 'b -> 'e) -> ('d, 'c) result -> 'a -> ('e, 'c) result

val pair : ('a, 'c) result * ('b, 'c) result -> ('a * 'b, 'c) result

val triple :
  ('a, 'c) result * ('b, 'c) result * ('d, 'c) result ->
  ('a * 'b * 'd, 'c) result

val lift_2_2 : ('a -> 'b) -> 'c * ('a, 'd) result -> ('c * 'b, 'd) result

val lift_2_1 : ('a -> 'b) -> ('a, 'c) result * 'd -> ('b * 'd, 'c) result
