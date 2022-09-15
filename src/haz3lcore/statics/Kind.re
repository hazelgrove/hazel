[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown
  | Abstract
  | Singleton(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type self =
  | Just(t)
  | Free;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Syn;
