open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

/* helper function for guarding options with is_valid */
let check_valid = (s, result) =>
  if (is_valid(s)) {
    result;
  } else {
    None;
  };

let is_true = eq("true");

let is_false = eq("false");

let is_let = eq("let");

let is_case = eq("case");

let is_wild = eq("_");

let split = (pos, name) => {
  let left_var = String.sub(name, 0, pos);
  let right_var = String.sub(name, pos, String.length(name) - pos);
  (left_var, right_var);
};
