module PaletteName =
 struct
  type t = string
  let eq = Util.str_eqb
  let rec _is_valid_internal = Var.is_valid
  let is_valid s =
    (* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
    if s = ""
    then false
    else (Util.char_eq_b first_char '$') && _is_valid_internal rest
  let check_valid s result =
    if is_valid s then result else None
 end
