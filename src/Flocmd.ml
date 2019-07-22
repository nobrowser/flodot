open Cmdliner

open Resultx.Monad

let eval_and_exit_annotated ti =
  let st =
    match Term.eval ti with
    | `Version | `Help -> 0
    | `Error `Term -> 1
    | `Error `Exn -> Term.exit_status_internal_error
    | `Error `Parse -> Term.exit_status_cli_error
    | `Ok (n, None) -> n
    | `Ok (n, Some msg) ->
       let _ = Printf.fprintf stderr "%s\n" msg in n
  in Stdlib.exit st

type errors =
  | System [@value 2]
  | Json
  | Inconsistent
  [@@deriving enum]

let errors_doc_alist =
  [(System, "A system error has occurred, such as a nonexistent input file.");
   (Json, "Error in the JSON parser applied to the input.");
   (Inconsistent, "The graph as specified by the input JSON file is inconsistent.")]

let errors_infos =
  let f (err, doc) = Term.exit_info ~doc (errors_to_enum err) in
  List.map f errors_doc_alist

let make_error err errstr =
  let n = errors_to_enum err in `Ok (n, Some errstr)

let run_check_consistency temp_required fname =
  try
  ( let ch = if String.equal fname "-" then stdin else open_in fname in
    ch |>
    Yojson.Basic.from_channel |>
    Flograph.of_json ~temp_required >>=
    Flograph.check_consistency |>
    Resultx.fold ~ok:(fun _ -> `Ok (0, None)) ~error:(fun e -> make_error Inconsistent e)
  ) with
  | Sys_error e -> make_error System e
  | Yojson.Json_error e -> make_error Json e

let vfname =
  Arg.(info [] ~docv:"FILE" |> pos 0 file "-" |> value)

let vtempreq =
  let doc = "Normally $(mname) will assign a default \"temperature\"
             (ie. priority) of $(b,Normal) to a node $(i,N) if $(i,N)
             has no $(b,temp) attribute.  But if $(opt) is specified,
             such nodes will be regarded as an error." in
  Arg.(info ["t"; "temp_required"] ~doc |> flag |> value)
  
let check_cmd =
  let doc = "Check consistency of dependency graph" in
  let man = [
  `S Manpage.s_description;
  `P "$(tname) verifies that the graph specified in $(i,FILE) is consistent.
      Apart from checking that every dependency specified also occurs as a node,
      and that every node name occurs exactly once, it also verifies that
      the graph is acyclic, that a node's \"temperature\" (i.e. priority)
      does not exceed the temperature of any of its dependencies, and that
      a node with a dependency in a state other than $(b,Done) is itself in
      the $(b,Blocked) state.";
  `P "To verify a graph passed via standard output (for example in a shell pipe)
      supply $(b,-) as the $(i,FILE) argument, or omit it completely."]
  in Term.(const run_check_consistency $ vtempreq $ vfname |> ret,
           info "Flodot_check" ~version:"v0.1.0" ~doc ~man           
           ~exits:(errors_infos @ default_exits))
