let export_id = 10000137;
//let export_slide = 1;
let export_str = "%EXPORT";
let _is_export = (==)(export_str);
let regexp = (r, s) =>
  Js_of_ocaml.Regexp.string_match(Js_of_ocaml.Regexp.regexp(r), s, 0)
  |> Option.is_some;
let is_export = regexp("^%EXPORT.$");

// get last character of string:
let get_export_offset = (s: string): int =>
  try(String.sub(s, String.length(s) - 1, 1) |> int_of_string) {
  | _ =>
    print_endline("get_export_offset error. s=" ++ s);
    0;
  };
