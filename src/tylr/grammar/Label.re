open Sexplib.Std;

// token classes determined by lexer.mll
// todo: add operator class
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Const(string)
  | Space
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit;

module Ord = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Ord);
module Set = Set.Make(Ord);

let is_empty =
  fun
  | Const("") => true
  | _ => false;

let is_const =
  fun
  | Const(_) => true
  | _ => false;

let length = _ => failwith("todo Label.length");

// succeeds on and duplicates labels of empty and dynamic length
// let unzip = (n: int, lbl: t): Result.t((t, t), Dir.t) =>
//   switch (lbl) {
//   | Const(t) when Token.length(t) > 0 =>
//     Token.unzip(n, t) |> Result.map(~f=((l, r)) => (Const(l), Const(r)))
//   | _ => Ok((lbl, lbl))
//   };
let unzip = (n: int, lbl: t) =>
  switch (lbl) {
  | Const(c) =>
    let l = String.sub(c, 0, n);
    let r = String.sub(c, n, length(c));
    (Const(l), Const(r));
  | _ => (lbl, lbl)
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

let is_complete = text =>
  fun
  | Space
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit =>
    // assuming text is consistent with lbl
    true
  | Const(c) => String.equal(c, text);
