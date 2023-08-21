let export_id =
  "10000137-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.get;

//let export_slide = 1;
let export_str = "EXPORT";
let _is_export = (==)(export_str);
let regexp = (r, s) =>
  Js_of_ocaml.Regexp.string_match(Js_of_ocaml.Regexp.regexp(r), s, 0)
  |> Option.is_some;
let is_export = regexp("^EXPORT$");
