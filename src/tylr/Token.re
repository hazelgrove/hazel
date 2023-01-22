open Sexplib.Std;
include String;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

module Shape = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Const(string)
    | Int_lit
    | Float_lit
    | Alphanum_lower
    | Alphanum_upper;

  let is_operand =
    fun
    | Int_lit
    | Float_lit
    | Alphanum_lower
    | Alphanum_upper => true
    | Const(_) => false;

  // order doesn't matter
  let to_int =
    fun
    | Const(_) => 0
    | Int_lit => 1
    | Float_lit => 2
    | Alphanum_lower => 3
    | Alphanum_upper => 4;

  let compare = (s1, s2) =>
    switch (s1, s2) {
    | (Const(s1), Const(s2)) => String.compare(s1, s2)
    | _ => Int.compare(to_int(s1), to_int(s2))
    };
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

// type t = {
//   id: Id.t,
//   text: string,
// };

// // effectful
// let mk = text => {
//   let id = Id.Gen.next();
//   {id, text};
