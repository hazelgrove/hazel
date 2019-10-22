open SemanticsCommon;
open GeneralUtil;

[@deriving sexp]
type t =
  | Cursor(cursor_position, TPat.t);

let erase = (Cursor(_, tpat)) => tpat;

let cursor_on_opseq = _ => false;

let move_cursor_left = _ => None;
let move_cursor_right = _ => None;

let place_after = tp =>
  switch (tp) {
  | TPat.Hole(_u) => Cursor(OnDelim(0, After), tp)
  | Var(v) => Cursor(OnText(Var.length(v)), tp)
  };

let place_before = tp =>
  switch (tp) {
  | TPat.Hole(_u) => Cursor(OnDelim(0, Before), tp)
  | Var(_) => Cursor(OnText(0), tp)
  };

let valid_cursors = (tp: TPat.t): list(cursor_position) =>
  switch (tp) {
  | Hole(_) => delim_cursors(0)
  | Var(v) => text_cursors(Var.length(v))
  };

let is_valid_cursor = (cursor: cursor_position, p: TPat.t): bool =>
  valid_cursors(p) |> contains(cursor);

let place_cursor = (cursor: cursor_position, tp: TPat.t): option(t) =>
  is_valid_cursor(cursor, tp) ? Some(Cursor(cursor, tp)) : None;
