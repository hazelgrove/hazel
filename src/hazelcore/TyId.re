open Sexplib.Std;

[@deriving sexp]
type t = string;

// TODO: What is the identity function
let to_string = x => x;
let of_string = x => x;

let eq = String.equal;

let length = String.length;

let valid_regex = Re.Str.regexp("^[_a-zA-Z][_a-zA-Z0-9']*$");

let is_valid = s => Re.Str.string_match(valid_regex, s, 0);

let is_Bool = eq("Bool");

let is_Int = eq("Int");

let is_Float = eq("Float");
