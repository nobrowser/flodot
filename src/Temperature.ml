type t =
  | Frozen
  | Cold
  | Normal
  | Hot
[@@deriving eq, ord, enum]

exception Invalid of string

let read s =
  if String.equal s "frozen" then Frozen
  else if String.equal s "cold" then Cold
  else if String.equal s "normal" then Normal
  else if String.equal s "hot" then Hot
  else raise (Invalid s)
