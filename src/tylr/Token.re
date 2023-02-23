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

let is_id_lower = regexp("^[a-z][A-Za-z0-9_]*$");
let is_id_upper = regexp("^[A-Z][A-Za-z0-9_]*$");

// do not call directly, use LangUtil.shape_of_token
let shape = (t: t): Shape.t =>
  switch (t) {
  | _ when is_id_lower(t) => Id_lower
  | _ when is_id_upper(t) => Id_upper
  | _ when is_int_lit(t) => Int_lit
  | _ when is_float_lit(t) => Float_lit
  | _ => Const(t)
  };

let split = (n, tok) => {
  let l = String.sub(tok, 0, n);
  let r = String.sub(tok, n, String.length(tok));
  (l, r);
};