open SemanticsCommon;

[@deriving sexp]
type t =
  | Cursor(cursor_position, TPat.t);

let erase = (Cursor(_, tpat)) => tpat;
