open Sm
open Lift

type t =
  Attributes of {t: Temperature.t; s: State.t; deps: string list}

let unassoc = function `Assoc x -> Ok x | _ -> Error "not a JSON assoc"
let unlist = function `List x -> Ok x | _ -> Error "not a JSON list"
let unstring = function `String x -> Ok x | _ -> Error "not a JSON string"

let extract_string_list jss =
  List.fold_left (Lift.fold unstring (Flip.f List.cons)) (Ok []) jss

let reader jt js jdeps =
  let rt = jt |> unstring |>>| Temperature.read in
  let rs = js |> unstring |>>| State.read in
  let rdeps = jdeps |> unlist |>>| extract_string_list in
  match rt, rs, rdeps with
  | Ok t, Ok s, Ok deps -> Ok (Attributes {t; s; deps})
  | (Error _ as e), _, _ -> e
  | _, (Error _ as e), _ -> e
  | _, _, (Error _ as e) -> e

let rec assoc q = function
  | [] -> None
  | (k, v) :: items ->
     if String.equal q k then Some v
     else assoc q items

let attributes_of_pairs ps =
  let t, s, deps = assoc "temp" ps, assoc "state" ps, assoc "deps" ps in
  match t, s, deps with
  | None, _ ,_ -> Error "temperature required but absent"
  | _, None, _ -> Error "state required but absent"
  | Some jt, Some js, None ->
     reader jt js (`List [])
  | Some jt, Some js, Some jl ->
     reader jt js jl

let attributes_of_json j = j |> unassoc |>>| attributes_of_pairs

let unlinked_nodes_of_pairs ps =
  let plumb (k, v) = match attributes_of_json v with
  | Error _ as e -> e
  | Ok v' -> Ok (k, v') in
  let madd m (k, v) = StringMap.add k v m in
  List.fold_left (Lift.fold plumb madd) (Ok StringMap.empty) ps

let check_duplicates ps =
  let rec check' m = function
    | [] -> false
    | (k, _) :: kvs -> StringMap.mem k m || check' (StringMap.add k true m) kvs in
  if check' StringMap.empty ps then Error "duplicate keys in map" else Ok ps

let unlinked_nodes_of_json j =
  j |> unassoc |>>| check_duplicates |>>| unlinked_nodes_of_pairs
