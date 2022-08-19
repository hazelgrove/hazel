[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Invalid
  | EmptyHole
  | Var(TyVar.t);
