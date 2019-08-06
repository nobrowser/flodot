open Cmdliner

type prog = (int * string option) Term.t * Term.info

val eval_and_exit_annotated : prog -> unit
val check_cmd : prog
val output_dot_cmd : prog
