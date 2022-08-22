open Sexplib.Std;

[@deriving (sexp, yojson)]
type t = string;

let length: t => int = String.length;

let equal: (t, t) => bool = String.equal;

let valid_regex = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");
let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

let is_reserved: string => bool =
  fun
  | "Int"
  | "Float"
  | "Bool" => true
  | s => Option.is_some(ExpandingKeyword.of_string(s));
