open Resultx.Monad

module SM = Sm.StringMap

type attributes =
  Attributes of {t: Temperature.t; s: State.t; deps: string list}

let make_attributes rt rs rdeps =
  rt >>= fun t ->
  rs >>= fun s ->
  rdeps >>= fun deps ->
  return (Attributes {t; s; deps})

let un_error str = Resultx.error ("not a JSON " ^ str)
let unassoc = function `Assoc x -> return x | _ -> un_error "assoc"
let unlist = function `List x -> return x | _ -> un_error "list"
let unstring = function `String x -> return x | _ -> un_error "string"

let extract_string_list j = unlist j >>| List.map unstring >>= Resultx.ljoin

let reader jt js jdeps =
  let rt = jt |> unstring |> Temperature.read in
  let rs = js |> unstring |> State.read in
  let rdeps = jdeps |> extract_string_list in
  make_attributes rt rs rdeps

let rec assoc q = function
  | [] -> Resultx.error (q ^ " required but absent")
  | (k, v) :: items ->
     if String.equal q k then return v
     else assoc q items

let rec assoc' q ~default = function
  | [] -> return default
  | (k, v) :: items ->
     if String.equal q k then return v
     else assoc' q default items

let attributes_of_pairs ps =
  assoc "temp" ps >>= fun jt ->
  assoc "state" ps >>= fun js ->
  assoc' "deps" (`List []) ps >>= fun jdeps ->
  reader jt js jdeps

let attributes_of_json j = j |> unassoc >>= attributes_of_pairs

let unlinked_nodes_of_pairs ps =
  let f = fun (k, j) -> (k, attributes_of_json j) in
  ps |> List.map f |> Sm.check_dupes

let check_missing m =
  let make_err l = Resultx.error (String.concat " " l) in
  let p _ n (Attributes {deps}) =
    match List.find_opt (fun dep -> not (SM.mem dep m)) deps with
    | Some dep -> make_err ["dependency"; dep; "of"; n; "is missing"]
    | None -> Ok () in
  Sm.StringMapRx.mfold p () m |> Resultx.map (fun _ -> m)

let unlinked_nodes_of_json j =
  j |> unassoc >>= unlinked_nodes_of_pairs >>= Sm.StringMapRx.mjoin >>= check_missing

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
