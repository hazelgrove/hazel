open Sexplib.Std;

[@deriving sexp]
type t = {
  row_height: float,
  col_width: float,
};
