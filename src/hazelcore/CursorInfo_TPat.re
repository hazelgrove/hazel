type cursor_term = CursorInfo_common.cursor_term;
type zoperand = CursorInfo_common.zoperand;

let rec extract_cursor_term = (ztp: ZTPat.t): cursor_term =>
  extract_from_operand(ztp)
and extract_from_operand = (ztp_operand: ZTPat.zoperand): cursor_term => {
  switch (ztp_operand) {
  | CursorP(cursor, operand) => TPat(cursor, operand)
  };
};

let get_zoperand_from_ztpat = (ztp: ZTPat.t): option(zoperand) =>
  Some(ZTPat(ztp));

let rec cursor_info =
        (~steps=[], ctx: Contexts.t, ztp: ZTPat.t)
        : option(CursorInfo_common.t) =>
  cursor_info_zoperand(~steps, ctx, ztp)
and cursor_info_zoperand =
    (
      ~steps: CursorPath_common.steps,
      ctx: Contexts.t,
      zoperand: ZTPat.zoperand,
    )
    : option(CursorInfo_common.t) => {
  let _ = steps;
  let cursor_term = extract_from_operand(zoperand);
  switch (zoperand) {
  | CursorP(_, EmptyHole(_)) =>
    Some(CursorInfo_common.mk(OnTPatHole, ctx, cursor_term))
  | CursorP(_, TyVar(_)) =>
    Some(CursorInfo_common.mk(OnTPat, ctx, cursor_term))
  };
};
