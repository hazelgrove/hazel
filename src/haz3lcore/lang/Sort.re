open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Any
  | Nul
  | Pat
  | Typ
  | Rul
  | Exp;

let root = Exp;

let all = [Any, Nul, Pat, Typ, Rul, Exp];

let eq = (==);

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | (Nul, _)
  | (_, Nul) => false
  | _ => s == s'
  };

let to_string =
  fun
  | Any => "Any"
  | Nul => "Nul"
  | Pat => "Pat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let ord = (s1: t, s2: t): option(Ord.t) =>
  switch (s1, s2) {
  | _ when eq(s1, s2) => Some(Eq)
  | (Typ, Pat)
  | (Pat, Exp)
  | (Typ, Exp) => Some(Lt)
  | (Pat, Typ)
  | (Exp, Pat)
  | (Exp, Typ) => Some(Gt)
  | _ => None
  };
