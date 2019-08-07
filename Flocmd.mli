open Cmdliner

type prog = (int * string option) Term.t * Term.info

val eval_and_exit_annotated : prog -> unit
val flodot_cmd : prog
