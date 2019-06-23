open Resultx
let ( >>= ) = bind
let ( >>| ) a f = map f a

module SM = Sm.StringMap

type attributes =
  Attributes of {t: Temperature.t; s: State.t; deps: string list}

let make_attributes rt rs rdeps =
  rt >>= fun t ->
  rs >>= fun s ->
  rdeps >>= fun deps ->
  Ok (Attributes {t; s; deps})

let unassoc = function `Assoc x -> Ok x | _ -> Error "not a JSON assoc"
let unlist = function  `List x -> Ok x | _ -> Error "not a JSON list"
let unstring = function `String x -> Ok x | _ -> Error "not a JSON string"

let extract_string_list j =
  let f r jstr =
    r >>= fun l ->
    unstring jstr >>= fun str ->
    Ok (str :: l) in
  unlist j >>= fun l ->
  List.fold_left f (Ok []) l
                                               
let reader jt js jdeps =
  let rt = jt |> unstring |> Temperature.read in
  let rs = js |> unstring |> State.read in
  let rdeps = jdeps |> extract_string_list in
  make_attributes rt rs rdeps

let rec assoc q = function
  | [] -> Error (q ^ " required but absent")
  | (k, v) :: items ->
     if String.equal q k then Ok v
     else assoc q items

let rec assoc' q ~default = function
  | [] -> Ok default
  | (k, v) :: items ->
     if String.equal q k then Ok v
     else assoc' q default items

let attributes_of_pairs ps =
  assoc "temp" ps >>= fun jt ->
  assoc "state" ps >>= fun js ->
  assoc' "deps" (`List []) ps >>= fun jdeps ->
  reader jt js jdeps

let attributes_of_json j = j |> unassoc >>= attributes_of_pairs

let unlinked_nodes_of_pairs ps =
  let f = fun (k, j) -> (k, attributes_of_json j) in
  ps |> List.map f |> Sm.of_results

let check_missing m =
  let check_node n (Attributes {deps}) = function
    | Error _ as e -> e
    | v ->
       ( match List.find_opt (fun dep -> not (SM.mem dep m)) deps with
         | Some dep ->
            Error (String.concat " " ["dependency"; dep; "of"; n; "is missing"])
         | None -> v
       ) in
  SM.fold check_node m (Ok m)

let unlinked_nodes_of_json j =
  j |> unassoc >>= unlinked_nodes_of_pairs >>= check_missing

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
  unlinked_nodes_of_json j >>| fun ns ->
  let vertices = SM.bindings ns in
  let edges =
    let add_vertex_edges es ((n, Attributes {deps}) as dest) =
      let add_edge es' n' = ((n', SM.find n' ns), dest) :: es' in
      List.fold_left add_edge es deps
      in List.fold_left add_vertex_edges [] vertices
    in (vertices, edges)
