[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | KindHole
  | Abstract
  | Singleton(Typ.t);
