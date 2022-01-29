[@deriving sexp]
type t =
  | CursorP(CursorPosition.t, TPat.t);

let erase =
  fun
  | CursorP(_, tpat) => tpat;

let place_after = (tpat: TPat.t): t =>
  switch (tpat) {
  | EmptyHole => CursorP(OnDelim(0, After), tpat)
  | TyVar(_, name) => CursorP(OnText(TyVar.Name.length(name)), tpat)
  };

let place_before = (tpat: TPat.t): t =>
  switch (tpat) {
  | EmptyHole => CursorP(OnDelim(0, Before), tpat)
  | TyVar(_, _) => CursorP(OnText(0), tpat)
  };

let valid_cursors: TPat.t => list(CursorPosition.t) =
  fun
  | EmptyHole => CursorPosition.delim_cursors_k(0)
  | TyVar(_, name) => CursorPosition.text_cursors(TyVar.Name.length(name));

let is_valid_cursor = (cursor: CursorPosition.t, tp: TPat.t): bool =>
  valid_cursors(tp) |> List.mem(cursor);

let place_cursor = (cursor: CursorPosition.t, tp: TPat.t): option(t) =>
  is_valid_cursor(cursor, tp) ? Some(CursorP(cursor, tp)) : None;

let is_after =
  fun
  | CursorP(cursor, EmptyHole) => cursor == OnDelim(0, After)
  | CursorP(cursor, TyVar(_, name)) =>
    cursor == OnText(TyVar.Name.length(name));

let is_before =
  fun
  | CursorP(cursor, EmptyHole) => cursor == OnDelim(0, Before)
  | CursorP(cursor, TyVar(_, _)) => cursor == OnText(0);

let move_cursor_left =
  fun
  | z when is_before(z) => None
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), e) => Some(CursorP(OnText(j - 1), e))
  | CursorP(_, TyVar(_)) => None
  | CursorP(OnDelim(k, After), z) => Some(CursorP(OnDelim(k, Before), z))
  | CursorP(OnDelim(_, Before), EmptyHole) => None;

let move_cursor_right =
  fun
  | z when is_after(z) => None
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), e) => Some(CursorP(OnText(j + 1), e))
  | CursorP(_, TyVar(_)) => None
  | CursorP(OnDelim(k, Before), z) => Some(CursorP(OnDelim(k, After), z))
  | CursorP(OnDelim(_, After), EmptyHole) => None;
