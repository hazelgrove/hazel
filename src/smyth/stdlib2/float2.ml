let to_string : float -> string =
 fun f ->
  let default = string_of_float f in
  if default.[String.length default - 1] = '.' then default ^ "0"
  else default
