open Sm

module M =
  Graph.Persistent.Digraph.ConcreteBidirectional (HashedString)

type t = M.t

let add_deps_edges s a g =
  let Unlinked.Attributes {deps} = a in
  let add_edge g s' = M.add_edge g s s' in
  List.fold_left add_edge g deps

let rec of_attributes attrs =
  StringMap.fold add_deps_edges attrs M.empty

