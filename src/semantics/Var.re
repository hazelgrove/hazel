[@deriving show({with_path: false})]
type t = string;

let eq = String.equal;

let valid_regex = Js_of_ocaml.Regexp.regexp("^[_a-z][_a-zA-Z0-9']*$");
let is_valid = s =>
  switch (Js_of_ocaml.Regexp.string_match(valid_regex, s, 0)) {
  | Some(_) => true
  | None => false
  };

/* helper function for guarding options with is_valid */
let check_valid = (s, result) =>
  if (is_valid(s)) {
    result;
  } else {
    None;
  };
