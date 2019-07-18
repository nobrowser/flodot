module Cmd = Cmdliner

open Resultx.Monad

let run_check_consistency fname =
  let ch = if String.equal fname "-" then stdin else open_in fname in
  ch |>
  Yojson.Basic.from_channel |>
  Flograph.of_json >>=
  Flograph.check_consistency |>
  Resultx.fold ~ok:(fun _ -> `Ok ()) ~error:(fun e -> failwith e)

let vfname =
  let open Cmd.Arg in
  info [] ~docv:"FILE" |> pos 0 file "-" |> value

let check_cmd =
  let doc = "Check consistency of dependency graph" in
  let man = [
  `S Cmd.Manpage.s_description;
  `P "$(tname) verifies that the graph specified in $(i,FILE) is consistent.
      Apart from checking that every dependency specified also occurs as a node,
      and that every node name occurs exactly once, it also verifies that
      the graph is acyclic, that a node's \"temperature\" (i.e. priority)
      does not exceed the temperature of any of its dependencies, and that
      a node with a dependency in a state other than Done is itself in
      the Blocked state.";
  `P "To verify a graph passed via standard output (for example in a shell pipe)
      supply \"-\" as the FILE argument, or omit it completely.";]
  in
  let open Cmd.Term in
  const run_check_consistency $ vfname,
  info "Flodot_check" ~version:"v0.1.0" ~doc ~exits:default_exits ~man
