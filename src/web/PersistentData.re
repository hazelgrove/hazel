open Util_web;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  scratch: (int, list((string, CellEditor.Model.persistent))),
  documentation: (int, list((string, CellEditor.Model.persistent))),
};
