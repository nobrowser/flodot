module Cmd = Cmdliner

open Resultx.Monad

type errors =
  | System [@value 1]
  | Json
  | Inconsistent
  [@@deriving enum]

let errors_doc_alist =
  [(errors_to_enum System,
    "A system error has occurred, such as a nonexistent input file.");
   (errors_to_enum Json,
    "Error in the JSON parser applied to the input.");
   (errors_to_enum Inconsistent,
    "The graph -- as specified by the input JSON file -- is inconsistent.")]

let errors_man_blocks =
  List.map (fun (e, t) -> `I (Printf.sprintf "%d" e, t)) errors_doc_alist

let make_error err errstr =
  let n = errors_to_enum err in
  `Error (false, Printf.sprintf "ERROR %d: %s" n errstr)

let run_check_consistency fname =
  try
  ( let ch = if String.equal fname "-" then stdin else open_in fname in
    ch |>
    Yojson.Basic.from_channel |>
    Flograph.of_json >>=
    Flograph.check_consistency |>
    Resultx.fold ~ok:(fun _ -> `Ok ()) ~error:(fun e -> make_error Inconsistent e)
  ) with
  | Sys_error e -> make_error System e
  | Yojson.Json_error e -> make_error Json e

let vfname =
  let open Cmd.Arg in
  info [] ~docv:"FILE" |> pos 0 file "-" |> value

let normal_error_info =
  let doc = "This exit status indicates a normal program error,
             whose code -- one of those docuemnted in the ERRORS section --
             will have been printed on standard error together with
             an explanatory message." in
  Cmd.Term.exit_info ~doc 1
  
let check_cmd =
  let doc = "Check consistency of dependency graph" in
  let man = [
  `S Cmd.Manpage.s_description;
  `P "$(tname) verifies that the graph specified in $(i,FILE) is consistent.
      Apart from checking that every dependency specified also occurs as a node,
      and that every node name occurs exactly once, it also verifies that
      the graph is acyclic, that a node's \"temperature\" (i.e. priority)
      does not exceed the temperature of any of its dependencies, and that
      a node with a dependency in a state other than $(b,Done) is itself in
      the $(b,Blocked) state.";
  `P "To verify a graph passed via standard output (for example in a shell pipe)
      supply $(b,-) as the $(i,FILE) argument, or omit it completely.";
  `S "ERRORS"; `Blocks errors_man_blocks]
  in
  let exits = normal_error_info :: Cmd.Term.default_exits in
  let open Cmd.Term in
  const run_check_consistency $ vfname |> ret,
  info "Flodot_check" ~version:"v0.1.0" ~doc ~exits ~man
