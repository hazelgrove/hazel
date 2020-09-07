[@deriving sexp]
type t = zoperand
and zoperand =
  | CursorP(CursorPosition.t, TPat.t);

let erase =
  fun
  | CursorP(_, tpat) => tpat;

let rec place_after = (tp: TPat.t): t => place_after_operand(tp)
and place_after_operand = (operand: TPat.operand): zoperand =>
  switch (operand) {
  | EmptyHole(_) => CursorP(OnDelim(0, After), operand)
  | TyVar(_, id) => CursorP(OnText(TyId.length(id)), operand)
  };

let rec place_before = (tp: TPat.t): t => place_before_operand(tp)
and place_before_operand = (operand: TPat.operand): zoperand =>
  switch (operand) {
  | EmptyHole(_) => CursorP(OnDelim(0, Before), operand)
  | TyVar(_, _) => CursorP(OnText(0), operand)
  };

let valid_cursors: TPat.t => list(CursorPosition.t) =
  fun
  | EmptyHole(_) => CursorPosition.delim_cursors_k(0)
  | TyVar(_, id) => CursorPosition.text_cursors(TyId.length(id));

let is_valid_cursor = (cursor: CursorPosition.t, tp: TPat.t): bool =>
  valid_cursors(tp) |> List.mem(cursor);

let place_cursor = (cursor: CursorPosition.t, tp: TPat.t): option(t) =>
  is_valid_cursor(cursor, tp) ? Some(CursorP(cursor, tp)) : None;

let rec is_after = (ztp: t): bool => ztp |> is_after_zoperand
and is_after_zoperand =
  fun
  | CursorP(cursor, EmptyHole(_)) => cursor == OnDelim(0, After)
  | CursorP(cursor, TyVar(_, id)) => cursor == OnText(TyId.length(id));

let rec is_before = (ztp: t): bool => ztp |> is_before_zoperand
and is_before_zoperand =
  fun
  | CursorP(cursor, EmptyHole(_)) => cursor == OnDelim(0, Before)
  | CursorP(cursor, TyVar(_, _)) => cursor == OnText(0);

let new_EmptyHole = (u_gen: MetaVarGen.t): (zoperand, MetaVarGen.t) => {
  let (hole, u_gen) = TPat.new_EmptyHole(u_gen);
  (place_before(hole), u_gen);
};

let rec move_cursor_left = (ztp: t): option(t) =>
  move_cursor_left_zoperand(ztp)
and move_cursor_left_zoperand =
  fun
  | z when is_before_zoperand(z) => None
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), e) => Some(CursorP(OnText(j - 1), e))
  | CursorP(_, TyVar(_)) => None
  | CursorP(OnDelim(k, After), z) => Some(CursorP(OnDelim(k, Before), z))
  | CursorP(OnDelim(_, Before), EmptyHole(_)) => None;

let rec move_cursor_right = (ztp: t): option(t) =>
  move_cursor_right_zoperand(ztp)
and move_cursor_right_zoperand =
  fun
  | z when is_after_zoperand(z) => None
  | CursorP(OnOp(_), _) => None
  | CursorP(OnText(j), e) => Some(CursorP(OnText(j + 1), e))
  | CursorP(_, TyVar(_)) => None
  | CursorP(OnDelim(k, Before), z) => Some(CursorP(OnDelim(k, After), z))
  | CursorP(OnDelim(_, After), EmptyHole(_)) => None;
