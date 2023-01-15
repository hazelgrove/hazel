open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Generation.t);

let empty = [];

let pop = _ => failwith("todo");
