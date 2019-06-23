open FutureResult

module StringMap = Map.Make(String)

let of_results l =
  let rec add1 m = function
  | [] -> Ok m
  | (_, (Error _ as e)) :: _ -> e
  | (k, Ok v) :: kvs ->
     if StringMap.mem k m then Error ("duplicate key " ^ k)
     else add1 (StringMap.add k v m) kvs in
  add1 StringMap.empty l
