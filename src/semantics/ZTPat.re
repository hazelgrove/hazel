open SemanticsCommon;

type t =
  | Cursor(cursor_side, TPat.t);

let erase = (Cursor(_, tpat)) => tpat;
