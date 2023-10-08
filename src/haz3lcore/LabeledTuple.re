open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let eq = String.equal;

let length = String.length;

let valid_regex =
  Re.Str.regexp("^\\([a-zA-Z]\\|_[_a-zA-Z0-9]\\)[_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);
