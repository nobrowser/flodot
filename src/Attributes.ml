open Resultx.Monad

type attributes =
  { t: Temperature.t
  ; s: State.t
  ; deps: string list
  }

let deps { deps = ds } = ds

let make_attributes rt rs rdeps =
  rt >>= fun t ->
  rs >>= fun s ->
  rdeps >>= fun deps ->
  return {t; s; deps}

module V =
  struct

  type t = string * attributes
  let equal (n, _) (n', _) = String.equal n n'
  let compare (n, _) (n', _) = String.compare n n'
  let hash (n, _) = HashedString.hash n

  type label = t
  let create n = n
  let label n = n

  end

let state (_, {s}) = s
let temperature (_, {t}) = t
let name (n, _) = n
