open Sexplib.Std;
include String;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t =
    | Const(string)
    | Id_lower
    | Id_upper
    | Int_lit
    | Float_lit;
};

// NOTE: keys are shapes, not the token strings
module Map = Map.Make(Shape);

let regexp = (r, s) => Re.Str.string_match(Re.Str.regexp(r), s, 0);

let is_arbitary_int = regexp("^[0-9_]*$");
let is_int_lit = str =>
  is_arbitary_int(str) && int_of_string_opt(str) != None;

let is_arbitary_float = x => x != "." && regexp("^[0-9]*\\.[0-9]*$", x);
let is_float_lit = str =>
  !is_arbitary_int(str)
  && is_arbitary_float(str)
  && float_of_string_opt(str) != None;

let is_alphanum_upper = regexp("^[A-Z][A-Za-z0-9_]*$");

let shape = (_: t): Shape.t => failwith("todo Token.shape");
