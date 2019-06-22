open Sm
module SM = Sm.StringMap

module M =
  Graph.Persistent.Digraph.ConcreteBidirectional (HashedString)

type t = M.t

let add_deps_edges s a g =
  let Unlinked.Attributes {deps} = a in
  match deps with
  | [] -> M.add_vertex g s
  | _ ->
     let add_edge g s' = M.add_edge g s' s in
     List.fold_left add_edge g deps

let of_attributes attribs =
  SM.fold add_deps_edges attribs M.empty

module Dfs =
  Graph.Traverse.Dfs (M)

let has_cycle g = Dfs.has_cycle g

let make_error l = Error (String.concat " " l)
  
let check_edge k1 k2 attribs =
  let Unlinked.Attributes a1, Unlinked.Attributes a2 =
    SM.find k1 attribs, SM.find k2 attribs in
  let s1, s2, t1, t2 = a1.s, a2.s, a1.t, a2.t in
  if t1 < t2 then make_error [k2; "is hotter than its dependency"; k1]
  else match s1, s2 with
       | _, State.Blocked -> Ok ()
       | State.Done, _ -> Ok ()
       | _, _ -> make_error [k2; "is blocked by"; k1]

let check_consistency attribs g =
  let check' k1 k2 = function
    | Error _ as e -> e
    | Ok () -> check_edge k1 k2 attribs in
  Ok () |> M.fold_edges check' g

