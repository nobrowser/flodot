open Resultx.Monad
open Attributes

module SM = Sm.StringMap

module type JSON_FLAGS =
  sig

  (* Is the temp attribute on nodes required, or should it default to Normal *)
  val temp_required : bool

  end

module type GRAPH_BUILDER =
  sig

  val graph : Yojson.Basic.t ->
              (Attributes.V.t list * (Attributes.V.t * Attributes.V.t) list, string)
              Resultx.t

  end

module Graph_builder (F : JSON_FLAGS) : GRAPH_BUILDER =
  struct

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
       else assoc' q ~default items

  let attributes_of_pairs ps =
    let temp =
      if F.temp_required then assoc "temp" ps
      else assoc' "temp" (`String "normal") ps in
    temp >>= fun jt ->
    assoc "state" ps >>= fun js ->
    assoc' "deps" (`List []) ps >>= fun jdeps ->
    reader jt js jdeps

  let attributes_of_json j = j |> unassoc >>= attributes_of_pairs

  let unlinked_nodes_of_pairs ps =
    let f = fun (k, j) -> (k, attributes_of_json j) in
    ps |> List.map f |> Sm.of_list_no_repeats

  let check_missing m =
    let make_err l = Resultx.error (String.concat " " l) in
    let p _ n a =
      match find_deps_opt ~f:(fun dep -> not (SM.mem dep m)) a with
      | Some dep -> make_err ["dependency"; dep; "of"; n; "is missing"]
      | None -> Ok () in
    Sm.StringMapRx.mfold p () m |> Resultx.map (fun _ -> m)

  let unlinked_nodes_of_json j =
    j |>
    unassoc >>=
    unlinked_nodes_of_pairs >>=
    Sm.StringMapRx.mjoin >>=
    check_missing

  let graph j =
    unlinked_nodes_of_json j >>| fun ns ->
    let vertices = SM.bindings ns in
    let edges =
      let add_vertex_edges es ((n, a) as dest) =
        let add_edge es' n' = ((n', SM.find n' ns), dest) :: es' in
        fold_left_deps ~f:add_edge es a
      in List.fold_left add_vertex_edges [] vertices
    in (vertices, edges)

  end
