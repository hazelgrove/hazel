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

let is_true_keyword = s => eq(s, "true");

let is_false_keyword = s => eq(s, "false");

let is_let_keyword = s => eq(s, "let");

let is_case_keyword = s => eq(s, "case");

let is_keyword = s => is_let_keyword(s) || is_case_keyword(s);
