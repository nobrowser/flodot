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
