type ctx =
  | Frozen
  | Cold
  | Hot
  | Done
  | Ready
  | Next
[@@deriving eq, ord, show { with_path = false }]

module Contexts =
  struct

  type t = ctx
  let equal = equal_ctx
  let compare = compare_ctx
  let show = show_ctx
  let pp = pp_ctx

  let read s =
    if String.equal s "Frozen" then Resultx.ok Frozen
    else if String.equal s "Cold" then Resultx.ok Cold
    else if String.equal s "Hot" then Resultx.ok Hot
    else if String.equal s "Done" then Resultx.ok Done
    else if String.equal s "Ready" then Resultx.ok Ready
    else if String.equal s "Next" then Resultx.ok Next
    else s ^ ": not a valid color context" |> Resultx.error

  end

module Flo_color_parser = Colorspec.Color_parser(Contexts)
