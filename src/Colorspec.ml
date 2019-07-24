open Resultx.Monad

module type CONTEXTS =
  sig

  include Map.OrderedType

  val read : string -> (t, string) Resultx.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  end

let int_res_of_string s =
  try int_of_string s |> return with Failure e -> Resultx.error e

let parse_color s =
  if Strutils.is_prefix "0x" s || Strutils.is_prefix "0X" s then
  ( int_res_of_string s )
  else if Strutils.is_prefix "#" s then
  ( let l = String.length s in
    let s' = String.sub s 1 (l - 1) in
    int_res_of_string ("0x" ^ s')
  ) else
  Resultx.error "parse_color: not a valid color spec"

let parse_spec reader s =
  match String.split_on_char ':' s with
  | [name; color] ->
     reader name >>= fun ctx ->
     parse_color color >>= fun cspec ->
     return (ctx, cspec)
  | _ -> Resultx.error "parse_spec: not a valid color spec"

let parse_specs reader s =
  let spec_list = String.split_on_char ',' s in
    match spec_list with
    | [""] -> return []
    | _ -> List.map (parse_spec reader) spec_list |> Resultx.ljoin

module type COLOR_PARSER =
  sig

  type ctx
  module CtxMap : Map.S with type key = ctx
  val parse : string -> (int CtxMap.t, string) Resultx.t
  val cmdliner_parse : string -> (int CtxMap.t, [`Msg of string]) Resultx.t
  val cmdliner_print : Format.formatter -> int CtxMap.t -> unit

  end

module Color_parser (Contexts: CONTEXTS) : COLOR_PARSER with type ctx = Contexts.t =
  struct

  type ctx = Contexts.t
  module CtxMap = Map.Make(Contexts)

  let map_from_pairs ps =
    ps |> List.to_seq |> CtxMap.of_seq |> return

  let parse specs =
    parse_specs Contexts.read specs >>= map_from_pairs

  let cmdliner_parse specs =
    let r = parse specs in
    Resultx.fold r ~ok:(fun v -> return v) ~error:(fun m -> Resultx.error (`Msg m))

  let cmdliner_print fmt m =
    let show_item (ctx, color) =
      Printf.sprintf "%s:0x%6.6x" (Contexts.show ctx) color in
    let items =
      CtxMap.to_seq m |>
      List.of_seq |>
      List.map show_item in
    match items with
    | [] -> Format.pp_print_string fmt "built-in"
    | _ ->
       let pp_item i s =
         if i <> 0 then (Format.pp_print_string fmt "," ; Format.pp_print_cut fmt ())
         else () ;
         Format.pp_print_string fmt s in
       List.iteri pp_item items

  end
