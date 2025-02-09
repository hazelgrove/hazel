open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Truth
  | Hole
  | Int(int)
  | Float(float)
  | String(string)
  | Ap(Constructor.t, option(t))
  | Tuple(list(t));
