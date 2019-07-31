(* abstract type of flow graph *)
type t

val of_json : temp_required:bool -> Yojson.Basic.t -> (t, string) result

val has_cycle : t -> bool

val check_consistency : t -> (t, string) result

val output_dot : Stdlib.out_channel ->
                 int Colorctx.Flo_color_parser.CtxMap.t ->
                 t ->
                 (unit, string) result
