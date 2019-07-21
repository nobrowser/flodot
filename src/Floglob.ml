
type t =
  {
  temp_required : bool
  }

let init = { temp_required = false }

let mod_temp_required s = fun t -> { t with temp_required = s }

let g = ref init

let set s = g := s

let get () = !g

let get_temp_required s = s.temp_required
