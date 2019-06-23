type t =
  | Done
  | Blocked
  | Ready
  | Next
[@@deriving eq, ord, enum]

exception Invalid of string
  
let read s =
  if String.equal s "done" then Done
  else if String.equal s "blocked" then Blocked
  else if String.equal s "ready" then Ready
  else if String.equal s "next" then Next
  else raise (Invalid s)
