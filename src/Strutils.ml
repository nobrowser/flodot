type tokstate = In | Out

let substrings_of_bounds s bs =
  List.map (fun (i, j) -> String.sub s i (j - i)) bs

let token_bounds ~f:f s =
  let l = String.length s in
  let rec bounds ac st = function
    | 0 -> ac
    | i ->
       if f s.[i - 1] then bounds ac Out (i - 1) else
       ( match st with
         | Out -> bounds ((i - 1, i) :: ac) In (i - 1)
         | In ->
            let (_, j), js = List.(hd ac, tl ac) in
            bounds ((i - 1, j) :: js) In (i - 1)
       ) in
  bounds [] Out l

let tokens ~f s = token_bounds ~f s |> substrings_of_bounds s

let field_bounds ~f:f s =
  let l = String.length s in
  let rec bounds ac st = function
    | 0 -> (match st with In -> ac | Out -> ((0, 0) :: ac))
    | i ->
       ( match st with
         | In ->
            if f s.[i - 1] then bounds ac Out (i - 1)
            else let (_, j), js = List.(hd ac, tl ac) in
                 bounds ((i - 1, j) :: js) In (i - 1)
         | Out ->
            if f s.[i - 1] then bounds ((i, i) :: ac) Out (i - 1)
            else bounds ac In (i - 1)
       ) in
  bounds [] Out l

let fields ~f s = field_bounds ~f s |> substrings_of_bounds s

let is_prefix p s =
  let lp, ls = String.(length p, length s) in
  lp <= ls && String.(equal p (sub s 0 lp))
