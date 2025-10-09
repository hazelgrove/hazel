open Util;
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type tpat_term('any) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list('any))
  | Var(string);
