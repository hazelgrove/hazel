open Sexplib.Std;

[@deriving (show({with_path: false}), sexp)]
type t = string;

let eq = String.equal;

let length = String.length;

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

let is_true = s => eq(s, "true");

let is_false = s => eq(s, "false");

let is_num = s => eq(s, "num");

let is_bool = s => eq(s, "bool");

let is_list = s => eq(s, "list");

let is_let = s => eq(s, "let");

let is_fn = s => eq(s, "fn");

let is_forall = s => eq(s, "forall");

let is_case = s => eq(s, "case");

let is_type = s => eq(s, "type");
