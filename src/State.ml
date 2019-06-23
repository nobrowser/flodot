open Resultx.Monad

type t =
  | Done
  | Blocked
  | Ready
  | Next
[@@deriving eq, ord, enum]

let read' s =
  if String.equal s "done" then return Done
  else if String.equal s "blocked" then return Blocked
  else if String.equal s "ready" then return Ready
  else if String.equal s "next" then return Next
  else Resultx.error ("invalid state " ^ s)

let read r = r >>= read'
