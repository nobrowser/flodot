let lift g = function
  | Error _ as e -> e
  | Ok x -> Ok (g x)

let (|>>) x g = (lift g) x

let chain g = function
  | Error _ as e -> e
  | Ok x -> g x

let (|>>|) x g = (chain g) x

let fold g f a x =
  match (a, x) with
  | (Error _ as e), _ -> e
  | Ok a', _ ->
     let f' x' = f a' x' in
     x |> g |>> f'

let pair = function
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok x, Ok y -> Ok (x, y)

let triple = function
  | (Error _ as e), _, _ -> e
  | _, (Error _ as e), _ -> e
  | _, _, (Error _ as e) -> e
  | Ok x, Ok y, Ok z -> Ok (x, y, z)
