open Util;

// token classes determined by lexer.mll
// todo: add operator class
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Const(Token.t)
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit;

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

let is_empty =
  fun
  | Const("") => true
  | _ => false;

let is_const =
  fun
  | Const(t) => Some(t)
  | _ => None;

let length = _ => failwith("todo Label.length");

// succeeds on and duplicates labels of empty and dynamic length
let unzip = (n: int, lbl: t): Result.t((t, t), Dir.t) =>
  switch (lbl) {
  | Const(t) when Token.length(t) > 0 =>
    Token.unzip(n, t) |> Result.map(~f=((l, r)) => (Const(l), Const(r)))
  | _ => Ok((lbl, lbl))
  };

let zip = (l: t, r: t): option(t) =>
  switch (l, r) {
  | (Const(l), Const(r)) => Some(Const(l ++ r))
  | _ when l == r => Some(l)
  | _ => None
  };

let consistent = (l: t, r: t): bool =>
  switch (l, r) {
  | (Const(""), _)
  | (_, Const("")) => true
  | _ => l == r
  };

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
