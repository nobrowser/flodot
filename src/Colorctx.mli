type ctx =
  | Frozen
  | Cold
  | Hot
  | Done
  | Ready
  | Next

module Contexts : Colorspec.CONTEXTS with type t = ctx

module Flo_color_parser : Colorspec.COLOR_PARSER with type ctx = Contexts.t
