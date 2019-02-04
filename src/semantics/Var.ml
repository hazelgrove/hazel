module Var =
 struct
  type t = string

  let eq = Util.str_eqb

  (* is_valid_internal s = true iff s is a string valid as the suffix of a variable *)
  let rec is_valid_suffix s =
    if s = ""
    then true
    else
      let ch = s.[0] in
      let rest = (String.sub s 1 (String.length s - 1))) in
      (
        (Util.char_eq_b ch '_') ||
        (Util.char_in_range_b ch 'a' 'z') ||
        (Util.char_in_range_b ch 'A' 'Z') ||
        (Util.char_in_range_b ch '0' '9') ||
        (Util.char_eq_b ch '\'')
      ) && is_valid_suffix rest

  (* is_valid s = true iff s is a valid variable *)
  (* should be equivalent to the OCaml rules: "[_a-z][_a-zA-Z0-9']*" *)
  let is_valid s =
    if s = ""
    then false
    else
      let first_char = s.[0] in
      let suffix = (String.sub s 1 (String.length s - 1))) in
      (
        (Util.char_eq_b first_char '_') ||
        (Util.char_in_range_b first_char 'a' 'z')
      ) && is_valid_suffix suffix

  (* helper function for guarding options with is_valid *)
  let check_valid s result =
    if is_valid s then result else None
 end
