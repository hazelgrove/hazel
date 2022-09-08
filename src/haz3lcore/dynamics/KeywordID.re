open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
let eq = (x: t, y: t) => x === y;

let init: t = 0;
