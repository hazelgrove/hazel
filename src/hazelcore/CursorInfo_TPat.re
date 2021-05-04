let extract_cursor_term =
  fun
  | ZTPat.CursorP(cursor, t) => CursorInfo.TPat(cursor, t);

let get_zoperand_from_ztpat =
    (ztp: ZTPat.t): option(CursorInfo_common.zoperand) =>
  Some(ZTPat(ztp));

let cursor_info =
    (~steps=[], ctx: Contexts.t, ztp: ZTPat.t): option(CursorInfo.t) => {
  let _ = steps;
  let cursor_term = extract_cursor_term(ztp);
  switch (ztp) {
  | CursorP(_, EmptyHole) =>
    Some(CursorInfo_common.mk(OnTPatHole, ctx, cursor_term))
  | CursorP(_, TyVar(_)) =>
    Some(CursorInfo_common.mk(OnTPat, ctx, cursor_term))
  };
};
