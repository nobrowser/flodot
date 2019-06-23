open Resultx.Monad

module M =
  Graph.Persistent.Digraph.ConcreteBidirectional (Unlinked.V)

type t = M.t

let of_json j =
  j |> Unlinked.graph >>= fun (vs, es) ->
  let vf = fun g -> List.fold_left M.add_vertex g vs in
  let add_edge' g (v1, v2) = M.add_edge g v1 v2 in
  let ef = fun g -> List.fold_left add_edge' g es in
  return (M.empty |> vf |> ef)

module Dfs =
  Graph.Traverse.Dfs (M)

let has_cycle g = Dfs.has_cycle g

exception Consistency of string list
                 
let check_edge v1 v2 =
  let n1 = Unlinked.name v1 in
  let n2 = Unlinked.name v2 in
  let s1 = Unlinked.state v1 in
  let s2 = Unlinked.state v2 in
  let t1 = Unlinked.temperature v1 in
  let t2 = Unlinked.temperature v2 in
  if t1 < t2 then raise (Consistency [n1; "is hotter than its dependency"; n2])
  else match s1, s2 with
       | _, State.Blocked -> ()
       | State.Done, _ -> ()
       | _, _ -> raise (Consistency [n1; "is blocked by"; n2])

let check_consistency g =
  try M.iter_edges check_edge g ; return g with
  | Consistency l -> Resultx.error (String.concat " " l)
