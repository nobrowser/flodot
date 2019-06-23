open FutureResult
let ( >>= ) = bind

type t =
  | Frozen
  | Cold
  | Normal
  | Hot
[@@deriving eq, ord, enum]

let read' s =
  if String.equal s "frozen" then Ok Frozen
  else if String.equal s "cold" then Ok Cold
  else if String.equal s "normal" then Ok Normal
  else if String.equal s "hot" then Ok Hot
  else Error ("invalid temperature " ^ s)

let read r = r >>= read'       
