open Util;
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type rul_term('exp, 'pat, 'any) =
  | Invalid(string)
  | MultiHole(list('any))
  | Rules('exp, list(('pat, 'exp)));
