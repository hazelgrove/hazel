open Sexplib.Std;

[@deriving sexp]
type t = string;

let eq = (x: t, y: t) => x == y;
let _is_valid_internal = Var.is_valid;
let valid_regex = Re.Str.regexp("^\\$[_a-zA-Z][_a-zA-Z0-9']*$");
let valid_regex_free = Re.Str.regexp("^\\$[_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);
let is_valid_free = s => Re.Str.string_match(valid_regex_free, s, 0);
let check_valid = (s, result) =>
  if (is_valid(s)) {
    result;
  } else {
    None;
  };
let strip_prefix = s => String.sub(s, 1, String.length(s) - 1);
let length = String.length;
