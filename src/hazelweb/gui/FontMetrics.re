open Sexplib.Std;

[@deriving sexp]
type t = {
  row_height: int,
  col_width: int,
};
