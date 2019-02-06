type t = string
let eq (x : t) (y : t) = x == y
let _is_valid_internal = Var.is_valid
let valid_regex = Js_of_ocaml.Regexp.regexp "^\\$[_a-zA-Z][_a-zA-Z0-9']*$"
let is_valid s = 
  begin match Js_of_ocaml.Regexp.string_match valid_regex s 0 with 
  | Some _ -> true
  | None -> false
  end
let check_valid s result =
  if is_valid s then result else None
