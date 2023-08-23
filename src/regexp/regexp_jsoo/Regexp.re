let regexp = (r, s) =>
  Js_of_ocaml.Regexp.string_match(Js_of_ocaml.Regexp.regexp(r), s, 0)
  |> Option.is_some;
