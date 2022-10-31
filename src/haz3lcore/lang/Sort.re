[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Any
  | Nul
  | Pat
  | Typ
  | TPat
  | TSum
  | Rul
  | Exp;

let root = Exp;

let all = [Any, Nul, TPat, Pat, Typ, Rul, Exp];

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
  | TPat => "TPat"
  | TSum => "TSum"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";
