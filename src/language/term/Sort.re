[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Any
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp
  | ModuleEntry
  | ModuleSignatureEntry;

let root = Exp;

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | _ => s == s'
  };

let to_string = show;

let to_string_verbose =
  fun
  | Any => "any"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression"
  | ModuleEntry => "module entry"
  | ModuleSignatureEntry => "module signature entry";
