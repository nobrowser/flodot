type ('a, 'e) t = ('a, 'e) result

let ok v = Ok v

let error e = Error e

let value r ~default =
  match r with
  | Ok v -> v
  | _ -> default

let get_ok = function
  | Ok v -> v
  | _ -> invalid_arg "Error value passed to get_ok"

let get_error = function
  | Error e -> e
  | _ -> invalid_arg "Ok value passed to get_error"

let bind r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let join = function
  | Ok r -> r
  | Error e -> Error e

let map f = function
  | Ok v -> Ok (f v)
  | Error e -> Error e

let map_error f = function
  | Ok v -> Ok v
  | Error e -> Error (f e)

let fold ~ok ~error = function
  | Ok v -> ok v
  | Error e -> error e

let iter c = function
  | Ok v -> c v
  | _ -> ()

let iter_error c = function
  | Error e -> c e
  | _ -> ()

let is_ok = function Ok _ -> true | Error _ -> false

let is_error = function Ok _ -> false | Error _ -> true

let equal ~ok ~error r1 r2 =
  match r1, r2 with
  | Ok _, Error _ | Error _, Ok _ -> false
  | Ok v1, Ok v2 -> ok v1 v2
  | Error e1, Error e2 -> error e1 e2

let compare ~ok ~error r1 r2 =
  match r1, r2 with
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1
  | Ok v1, Ok v2 -> ok v1 v2
  | Error e1, Error e2 -> error e1 e2

let to_option = function
  | Ok v -> Some v
  | _ -> None

let to_list = function
  | Ok v -> [v]
  | _ -> []

let to_seq = function
  | Ok v -> Seq.return v
  | _ -> Seq.empty

let ljoin (type e) l =
  let module J = struct exception Join of e end in
  let f a = function
    | Ok v -> v :: a
    | Error e -> raise (J.Join e) in
  try Ok (List.fold_left f [] l) with J.Join e -> Error e

let mjoin (type e) m =
  let module J = struct exception Join of e end in
  let f k r a = match r with
  | Ok v -> Sm.StringMap.add k v a
  | Error e -> raise (J.Join e) in
  try Ok (Sm.StringMap.fold f m (Sm.StringMap.empty)) with J.Join e -> Error e

module type MONAD =
  sig

  val return : 'a -> ('a, 'e) t
  
  val ( >>= ) : ('a, 'e) t ->
                ('a -> ('b, 'e) t) -> ('b, 'e) t

  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end
                                                                     
module Monad : MONAD =
  struct

  let return = ok
  
  let ( >>= ) = bind

  let ( >>| ) a f = map f a

  end
