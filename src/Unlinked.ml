module SM = Sm.StringMap

exception Invalid of Yojson.Basic.t * string

let invalid j s = raise (Invalid (j, s))

type attributes =
  Attributes of {t: Temperature.t; s: State.t; deps: string list}

let unassoc j =
  match j with
  | `Assoc x -> x
  | _ -> invalid j "not a JSON assoc"

let unlist j =
  match j with
  | `List x -> x
  | _ ->  invalid j "not a JSON list"

let unstring j =
  match j with
  | `String x -> x
  | _ -> invalid j "not a JSON string"

let reader jt js jdeps =
  let t = jt |> unstring |> Temperature.read in
  let s = js |> unstring |> State.read in
  let deps = jdeps |> unlist |> List.map unstring in
  Attributes {t; s; deps}

let rec assoc q = function
  | [] -> None
  | (k, v) :: items ->
     if String.equal q k then Some v
     else assoc q items

let attributes_of_pairs j ps =
  let t, s, deps = assoc "temp" ps, assoc "state" ps, assoc "deps" ps in
  match t, s, deps with
  | None, _ ,_ -> invalid j "temperature required but absent"
  | _, None, _ -> invalid j "state required but absent"
  | Some jt, Some js, None ->
     reader jt js (`List [])
  | Some jt, Some js, Some jl ->
     reader jt js jl

let attributes_of_json j = j |> unassoc |> attributes_of_pairs j

let unlinked_nodes_of_pairs ps =
  let f = fun (k, j) -> (k, attributes_of_json j) in
  ps |> List.map f |> List.to_seq |> SM.of_seq

let check_duplicates j ps =
  let rec check' m = function
    | [] -> false
    | (k, _) :: kvs -> SM.mem k m || check' (SM.add k true m) kvs in
  if check' SM.empty ps then invalid j "duplicate keys in map" else ps

let unlinked_nodes_of_json j =
  j |> unassoc |> check_duplicates j |> unlinked_nodes_of_pairs

module V =
  struct

  type t = string * attributes
  let equal (n, _) (n', _) = String.equal n n'
  let compare (n, _) (n', _) = String.compare n n'
  let hash (n, _) = HashedString.hash n

  type label = t
  let create n = n
  let label n = n

  end

let state (_, Attributes {s}) = s
let temperature (_, Attributes {t}) = t
let name (n, _) = n

let graph j =
  try
  ( let l = unlinked_nodes_of_json j in
    let vertices = SM.bindings l in
    let edges =
      let add_vertex_edges es ((n, Attributes {deps}) as dest) =
        let add_edge es' n' = ((n', Sm.find' n' l), dest) :: es' in
        List.fold_left add_edge es deps
      in List.fold_left add_vertex_edges [] vertices
    in Ok (vertices, edges)
  ) with
  | Invalid (j, s) -> Error (j, s)
  | Temperature.Invalid s -> Error (`String s, "invalid temperature")
  | State.Invalid s -> Error (`String s, "invalid state")
  | Sm.Not_found_n n -> Error (`String n, "not found")
