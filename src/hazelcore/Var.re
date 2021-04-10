open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_var_regex = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");

let valid_operator_regex = Re.Str.regexp("^[_]?[&*+-/:;<=>?@^|~]*[_]?$");
let valid_expression_operator_regex = Re.Str.regexp("^[&*+-/:;<=>?@^|~]*$");
let valid_complete_operator_regex =
  Re.Str.regexp("^[_][&*+-/:;<=>?@^|~]*[_]$");

let is_operator = s => {
  Re.Str.string_match(valid_operator_regex, s, 0) && String.length(s) > 0;
};

let is_exp_operator = s => {
  Re.Str.string_match(valid_expression_operator_regex, s, 0)
  && String.length(s) > 0;
};

let is_valid = s => {
  Re.Str.string_match(valid_var_regex, s, 0) || is_operator(s);
};

let is_valid_operator = s => {
  Re.Str.string_match(valid_operator_regex, s, 0);
};

let is_complete_operator = s => {
  Re.Str.string_match(valid_complete_operator_regex, s, 0);
};

let is_incomplete_operator = s => {
  is_operator(s)
  && s != "_"
  && !Re.Str.string_match(valid_operator_regex, s, 0);
};

/* helper function for guarding options with is_valid */
let check_valid = (s, result) =>
  if (is_valid(s)) {
    result;
  } else {
    None;
  };

/* helper function for guarding options with is_valid */
let check_valid_operator = (s, result) =>
  if (is_valid_operator(s)) {
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

let surround_underscore = s => "_" ++ s ++ "_";

let remove_underscores = s => String.sub(s, 0, String.length(s) - 1);
