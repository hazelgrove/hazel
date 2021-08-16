open Sexplib.Std;

[@deriving (sexp, show)]
type t = string;

let eq = (x: t, y: t) => x == y;
let valid_regex = Re.Str.regexp("^\\$[_a-zA-Z][_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);
let check_valid = (s, result) =>
  if (is_valid(s)) {
    result;
  } else {
    None;
  };
