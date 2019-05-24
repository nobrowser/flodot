type t =
  | Ready
  | Blocked
  | Next
  | Done

let read s =
  if String.equal s "ready" then Ok Ready
  else if String.equal s "blocked" then Ok Blocked
  else if String.equal s "next" then Ok Next
  else if String.equal s "done" then Ok Done
  else Error ("invalid state: " ^ s)
