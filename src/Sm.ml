module StringMap = Map.Make(String)

exception Ex of string

let check_dupes l =
  let rec add1 m = function
  | [] -> m
  | (k, v) :: kvs ->
     if StringMap.mem k m then raise (Ex k)
     else add1 (StringMap.add k v m) kvs in
  add1 StringMap.empty l
