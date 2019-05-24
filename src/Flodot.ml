open Sm

module G : Graph.Topological.G =
  struct

  type t = Unlinked.t StringMap.t
  module V = HashedString

  let iter_vertex cmd m = StringMap.iter (fun k _ -> cmd k) m

  let iter_succ cmd m k =
    if StringMap.mem k m then
      let Unlinked.Attributes {deps} = StringMap.find k m in
      List.iter cmd deps
    else raise (Invalid_argument ("vertex " ^ k ^ " not in graph"))

  end



