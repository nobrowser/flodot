open Resultx.Monad

type t =
  | Frozen
  | Cold
  | Normal
  | Hot
[@@deriving eq, ord, enum]

let read' s =
  if String.equal s "frozen" then return Frozen
  else if String.equal s "cold" then return Cold
  else if String.equal s "normal" then return Normal
  else if String.equal s "hot" then return Hot
  else Resultx.error ("invalid temperature " ^ s)

let read r = r >>= read'       
