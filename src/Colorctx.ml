open Resultx.Monad

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
    if String.equal s "Frozen" then return Frozen
    else if String.equal s "Cold" then return Cold
    else if String.equal s "Hot" then return Hot
    else if String.equal s "Done" then return Done
    else if String.equal s "Ready" then return Ready
    else if String.equal s "Next" then return Next
    else s ^ ": not a valid color context" |> Resultx.error

  end

module Flo_color_parser = Colorspec.Color_parser(Contexts)
