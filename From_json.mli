module type JSON_FLAGS =
  sig

  (* Is the temp attribute on nodes required, or should it default to Normal *)
  val temp_required : bool

  end

module type GRAPH_BUILDER =
  sig

  val graph : Yojson.Basic.t ->
              (Attributes.V.t list * (Attributes.V.t * Attributes.V.t) list, string)
              result

  end

module Graph_builder (F : JSON_FLAGS) : GRAPH_BUILDER
