let is_js : bool =
  match Sys.backend_type with Sys.Other "js_of_ocaml" -> true | _ -> false
