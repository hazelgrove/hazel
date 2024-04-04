open Sexplib.Std;

// token classes determined by lexer.mll
// todo: add operator class
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Const(Padding.t, string)
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

let const = (~padding=Padding.none, text) => Const(padding, text);

let padding =
  fun
  | Const(padding, _) => padding
  | _ => Padding.none;

let is_empty =
  fun
  | Const(_, "") => true
  | _ => false;

let is_const =
  fun
  | Const(_) => true
  | _ => false;

let is_complete = text =>
  fun
  | Space
  | Id_lower
  | Id_upper
  | Int_lit
  | Float_lit =>
    // assuming text is consistent with lbl
    true
  | Const(_, c) => String.equal(c, text);
