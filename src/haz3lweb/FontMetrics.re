open Sexplib.Std;

[@warning "-33"]
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  row_height: float,
  col_width: float,
};

let init = {row_height: 10., col_width: 10.};
