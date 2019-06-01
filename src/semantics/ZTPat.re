open SemanticsCommon;

[@deriving (show, sexp)]
type t =
  | Cursor(cursor_side, TPat.t);

let erase = (Cursor(_, tpat)) => tpat;
