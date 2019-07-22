open Cmdliner

val eval_and_exit_annotated : (int * string option) Term.t * Term.info -> unit
val check_cmd : (int * string option) Term.t * Term.info
