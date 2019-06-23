open FutureResult

module M =
  Graph.Persistent.Digraph.ConcreteBidirectional (Unlinked.V)

type t = M.t

let of_json j =
  match Unlinked.graph j with
  | Error _ as e -> e
  | Ok (vs, es) ->
     let vf = fun g -> List.fold_left M.add_vertex g vs in
     let add_edge' g (v1, v2) = M.add_edge g v1 v2 in
     let ef = fun g -> List.fold_left add_edge' g es in
     Ok (M.empty |> vf |> ef)

module Dfs =
  Graph.Traverse.Dfs (M)

let has_cycle g = Dfs.has_cycle g

let make_error l = Error (String.concat " " l)
  
let check_edge v1 v2 =
  let n1 = Unlinked.name v1 in
  let n2 = Unlinked.name v2 in
  let s1 = Unlinked.state v1 in
  let s2 = Unlinked.state v2 in
  let t1 = Unlinked.temperature v1 in
  let t2 = Unlinked.temperature v2 in
  if t1 < t2 then make_error [n1; "is hotter than its dependency"; n1]
  else match s1, s2 with
       | _, State.Blocked -> Ok ()
       | State.Done, _ -> Ok ()
       | _, _ -> make_error [n2; "is blocked by"; n1]

let check_consistency g =
  let check' v1 v2 = function
    | Error _ as e -> e
    | Ok () -> check_edge v1 v2 in
  Ok () |> M.fold_edges check' g
