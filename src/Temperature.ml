type t =
  | Normal
  | Frozen
  | Cold
  | Hot

let read s =
  if String.equal s "normal" then Ok Normal
  else if String.equal s "frozen" then Ok Frozen
  else if String.equal s "cold" then Ok Cold
  else if String.equal s "hot" then Ok Hot
  else Error ("invalid temperature: " ^ s)
