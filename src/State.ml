open Resultx
let ( >>= ) = bind

type t =
  | Done
  | Blocked
  | Ready
  | Next
[@@deriving eq, ord, enum]

let read' s =
  if String.equal s "done" then Ok Done
  else if String.equal s "blocked" then Ok Blocked
  else if String.equal s "ready" then Ok Ready
  else if String.equal s "next" then Ok Next
  else Error ("invalid state " ^ s)

let read r = r >>= read'
