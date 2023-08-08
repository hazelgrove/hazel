open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  dynamics: bool,
};

let off: t = {statics: false, dynamics: false};
let on: t = {statics: true, dynamics: true};
