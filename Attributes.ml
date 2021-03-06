open Aaa.Resultx.Monad

type attributes =
  { t: Temperature.t
  ; s: State.t
  ; deps: string list
  }

let find_deps_opt ~f {deps} = List.find_opt f deps

let fold_left_deps ~f a {deps} = List.fold_left f a deps

let make_attributes rt rs rdeps =
  rt >>= fun t ->
  rs >>= fun s ->
  rdeps >>| fun deps ->
  {t; s; deps}

module V =
  struct

  type t = string * attributes
  let equal (n, _) (n', _) = String.equal n n'
  let compare (n, _) (n', _) = String.compare n n'
  let hash (n, _) = Aaa.HashedString.hash n

  type label = t
  let create n = n
  let label n = n

  end

let state (_, {s}) = s
let temperature (_, {t}) = t
let name (n, _) = n
