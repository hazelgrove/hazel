open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  dynamics: bool,
};

let off: t = {statics: false, elaborate: false, dynamics: false};
let on: t = {statics: true, elaborate: true, dynamics: true};
