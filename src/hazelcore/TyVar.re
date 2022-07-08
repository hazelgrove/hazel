open Sexplib.Std;

[@deriving sexp]
type t = string;

let length: t => int = String.length;

let equal: (t, t) => bool = String.equal;

let valid_name: string => bool = {
  let re = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");
  s => Re.Str.string_match(re, s, 0);
};

let builtin_type: string => bool =
  fun
  | "Int"
  | "Float"
  | "Bool" => true
  | _ => false;

let keyword: string => bool =
  fun
  | "let"
  | "case"
  | "type"
  | "fun" => true
  | _ => false;

let reserved_word = (s: string): bool => builtin_type(s) || keyword(s);
