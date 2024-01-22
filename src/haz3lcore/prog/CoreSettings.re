open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  inference: bool,
};

let off: t = {
  statics: false,
  elaborate: false,
  assist: false,
  dynamics: false,
  inference: false,
};
let on: t = {
  statics: true,
  elaborate: true,
  assist: true,
  dynamics: true,
  inference: true,
};
