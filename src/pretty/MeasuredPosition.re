open Util;

[@deriving sexp]
type t = {
  row: int,
  col: int,
};

let compare = (pos1, pos2) =>
  if (pos1.row < pos2.row) {
    (-1);
  } else if (pos1.row > pos2.row) {
    1;
  } else {
    Int.compare(pos1.col, pos2.col);
  };

let zero = {row: 0, col: 0};
