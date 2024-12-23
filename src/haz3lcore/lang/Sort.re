[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Drv(DrvSort.t)
  | Any
  | Nul
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

let root = Exp;

let all =
  (DrvSort.all |> List.map(s => Drv(s)))
  @ [Any, Nul, Pat, Typ, Rul, Exp, TPat];

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
  | Drv(s) => DrvSort.class_of(s)
  | Any => "Any"
  | Nul => "Nul"
  | Pat => "Pat"
  | TPat => "TPat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let to_string_verbose =
  fun
  | Drv(s) => DrvSort.to_string_verbose(s)
  | Any => "any"
  | Nul => "null"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression";
