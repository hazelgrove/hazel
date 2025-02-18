open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  current: int,
  scratchpads: list((string, CellEditor.Model.t)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = (int, list((string, CellEditor.Model.persistent)));
