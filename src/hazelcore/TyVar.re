open Sexplib.Std;

[@deriving sexp]
type t = string;

let equal: (t, t) => bool = String.equal;

let valid_name: string => bool = {
  let re = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");
  s => Re.Str.string_match(re, s, 0);
};

let reserved_word = (s: string): bool =>
  // built-in types
  s == "Int"
  || s == "Float"
  || s == "Bool"
  // keywords
  || s == "let"
  || s == "case";
