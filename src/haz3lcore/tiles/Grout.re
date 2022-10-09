open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(list(Whitespace.t), Hole.t);
