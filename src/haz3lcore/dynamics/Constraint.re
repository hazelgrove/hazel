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

let nil = Ap("nil", None);
let cons = (hd, tl) => Ap("cons", Some(Tuple([hd, tl])));

let true_ = Ap("true", None);
let false_ = Ap("false", None);
