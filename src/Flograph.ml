open Resultx.Monad

module M =
  Graph.Persistent.Digraph.ConcreteBidirectional (Attributes.V)

type t = M.t

let of_json ~temp_required j =
  let module B =
    From_json.Graph_builder(struct let temp_required = temp_required end) in
  j |> B.graph >>= fun (vs, es) ->
  let vf = fun g -> List.fold_left M.add_vertex g vs in
  let add_edge' g (v1, v2) = M.add_edge g v1 v2 in
  let ef = fun g -> List.fold_left add_edge' g es in
  return (M.empty |> vf |> ef)

module Dfs =
  Graph.Traverse.Dfs (M)

let has_cycle g = Dfs.has_cycle g

exception Inconsistent of string list

let raise_hotter n1 n2 =
  raise (Inconsistent [n2; "is hotter than its dependency"; n1])

let raise_blocked n1 n2 =
  raise (Inconsistent [n2; "is blocked by"; n1])

let check_edge v1 v2 =
  let n1 = Attributes.name v1 in
  let n2 = Attributes.name v2 in
  let s1 = Attributes.state v1 in
  let s2 = Attributes.state v2 in
  let t1 = Attributes.temperature v1 in
  let t2 = Attributes.temperature v2 in
  if Temperature.compare t1 t2 < 0 then raise_hotter n1 n2
  else match s1, s2 with
       | _, State.Blocked -> ()
       | State.Done, _ -> ()
       | _, _ -> raise_blocked n1 n2

let check_consistency g =
  try M.iter_edges check_edge g ; return g with
  | Inconsistent l -> Resultx.error (String.concat " " l)

module Dot_params =
  struct

  type t = M.t
  module V = M.V
  module E = M.E
  let iter_vertex = M.iter_vertex
  let iter_edges_e = M.iter_edges_e

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name v = Attributes.name v
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []

  (* todo: look up the colors from names in rgb.txt *)
  let vertex_attributes v =
    let state_attributes =
      match Attributes.state v with
      | State.Blocked -> [`Shape `Box]
      | State.Done -> [`Shape `Oval; `Color 0xbebebe]
      | State.Ready -> [`Shape `Oval; `Color 0x00cc00]
      | State.Next -> [`Shape `Oval; `Color 0xcc0000] in
    let temp_attributes =
      match Attributes.temperature v with
      | Temperature.Normal -> []
      | Temperature.Cold -> [`Fontcolor 0x20b2aa]
      | Temperature.Frozen -> [`Fontcolor 0x1874cd]
      | Temperature.Hot -> [`Fontcolor 0xb22222] in
    state_attributes @ temp_attributes

  end

module Dot_engine = Graph.Graphviz.Dot (Dot_params)

let output_dot outch g =
  Ok (Dot_engine.output_graph outch g; output_char outch '\n'; flush outch)
