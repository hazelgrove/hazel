[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Drv(DrvSort.t)
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp;

let show =
  fun
  | Drv(s) => DrvSort.show(s)
  | _ as s => show(s);

let class_of =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | _ as s => show(s);

type gadt('a) =
  | DrvSort: gadt(DrvTermBase.Any.t)
  | AnySort: gadt(TermBase.Any.t)
  | PatSort: gadt(TermBase.Pat.t)
  | TypSort: gadt(TermBase.Typ.t)
  | TPatSort: gadt(TermBase.TPat.t)
  | RulSort: gadt(TermBase.Rul.t)
  | ExpSort: gadt(TermBase.Exp.t);

let root = Exp;

let all =
  (DrvSort.all |> List.map(s => Drv(s))) @ [Any, Pat, Typ, Rul, Exp, TPat];

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | (Drv(s), Drv(s')) => DrvSort.consistent(s, s')
  | (Drv(_), _) => false
  | _ => s == s'
  };

let to_string =
  fun
  | Drv(s) => DrvSort.class_of(s)
  | Any => "Any"
  | Pat => "Pat"
  | TPat => "TPat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let to_string_verbose =
  fun
  | Drv(s) => DrvSort.to_string_verbose(s)
  | Any => "any"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression";
