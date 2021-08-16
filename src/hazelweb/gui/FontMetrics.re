open Sexplib.Std;

[@deriving (sexp, show)]
type t = {
  row_height: float,
  col_width: float,
};
