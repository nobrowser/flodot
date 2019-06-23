module StringMap = Map.Make(String)

exception Not_found_n of string

let find' k m =
  match StringMap.find_opt k m with
  | None -> raise (Not_found_n k)
  | Some v -> v
