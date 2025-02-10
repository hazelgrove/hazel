[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp;

type gadt('a) =
  | AnySort: gadt(TermBase.Any.t)
  | PatSort: gadt(TermBase.Pat.t)
  | TypSort: gadt(TermBase.Typ.t)
  | TPatSort: gadt(TermBase.TPat.t)
  | RulSort: gadt(TermBase.Rul.t)
  | ExpSort: gadt(TermBase.Exp.t);

let root = Exp;

let all = [Any, Pat, Typ, Rul, Exp, TPat];

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | _ => s == s'
  };

let to_string =
  fun
  | Any => "Any"
  | Pat => "Pat"
  | TPat => "TPat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let to_string_verbose =
  fun
  | Any => "any"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression";
