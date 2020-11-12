open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_regex = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");

let operator_regex_pat = Re.Str.regexp("^[_]?[&*+-/:;<=>?@^|~]+[_]?$");
let operator_regex_exp = Re.Str.regexp("^[&*+-/:;<=>?@^|~]*$");
let operator_regex_pat_complete = Re.Str.regexp("^[_][&*+-/:;<=>?@^|~]+[_]$");

let is_valid = s => {
  Re.Str.string_match(valid_regex, s, 0);
};

let is_valid_operator = s => {
  Re.Str.string_match(operator_regex_pat, s, 0);
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

let is_operator = s => Re.Str.string_match(operator_regex_pat, s, 0);

let split = (pos, name) => {
  let left_var = String.sub(name, 0, pos);
  let right_var = String.sub(name, pos, String.length(name) - pos);
  (left_var, right_var);
};

/* return the user op w/out underscores if the pattern is valid
   i.e. matches _<some ops>_ */
let extract_op_exp = s =>
  if (Re.Str.string_match(operator_regex_pat_complete, s, 0)) {
    // Trim off the underscores
    String.sub(s, 1, String.length(s) - 2);
  } else if (Re.Str.string_match(operator_regex_pat, s, 0)) {
    // Return same string if valid
    Re.Str.matched_string(s);
  } else {
    print_endline(s);
    failwith("invalid user op definition");
  };
