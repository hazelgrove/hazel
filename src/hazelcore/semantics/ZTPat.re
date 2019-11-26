open GeneralUtil;

[@deriving sexp]
type t =
  | Cursor(CursorPosition.t, TPat.t);

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

let valid_cursors = (tp: TPat.t): list(CursorPosition.t) =>
  switch (tp) {
  | Hole(_) => CursorPosition.delim_cursors(0)
  | Var(v) => CursorPosition.text_cursors(Var.length(v))
  };

let is_valid_cursor = (cursor: CursorPosition.t, p: TPat.t): bool =>
  valid_cursors(p) |> contains(cursor);

let place_cursor = (cursor: CursorPosition.t, tp: TPat.t): option(t) =>
  is_valid_cursor(cursor, tp) ? Some(Cursor(cursor, tp)) : None;

let is_before = (Cursor(cursor, tpat)) =>
  switch (tpat) {
  | Hole(_) => cursor == OnDelim(0, Before)
  | Var(_) => cursor == OnText(0)
  };

let is_after = (Cursor(cursor, tpat)) =>
  switch (tpat) {
  | Hole(_) => cursor == OnDelim(0, After)
  | Var(x) => cursor == OnText(Var.length(x))
  };
