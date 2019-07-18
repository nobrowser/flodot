module Cmd : module type of Cmdliner

val check_cmd : unit Cmd.Term.result Cmd.Term.t * Cmd.Term.info
