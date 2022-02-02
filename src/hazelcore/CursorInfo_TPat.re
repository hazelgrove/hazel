let extract_cursor_term =
  fun
  | ZTPat.CursorP(cursor, t) => CursorInfo.TPat(cursor, t);

let get_zoperand_from_ztpat =
    (ztp: ZTPat.t): option(CursorInfo_common.zoperand) =>
  Some(ZTPat(ztp));

let cursor_info =
    (~steps=[], ctx: Contexts.t, u_gen: MetaVarGen.t, ztp: ZTPat.t)
    : option(CursorInfo_common.deferrable(CursorInfo.t)) => {
  let _ = steps;
  let cursor_term = extract_cursor_term(ztp);
  switch (ztp) {
  | CursorP(_, EmptyHole) =>
    Some(
      CursorInfo_common.CursorNotOnDeferredVarPat(
        CursorInfo_common.mk(OnTPatHole, ctx, u_gen, cursor_term),
      ),
    )
  | CursorP(_, TyVar(NotInHole, tyid)) =>
    Some(
      CursorInfo_common.CursorOnDeferredTyVarPat(
        tyuses =>
          CursorInfo_common.mk(
            ~tyuses,
            OnTPat(None),
            ctx,
            u_gen,
            cursor_term,
          ),
        tyid,
      ),
    )
  | CursorP(_, TyVar(status, _)) =>
    Some(
      CursorInfo_common.CursorNotOnDeferredVarPat(
        CursorInfo_common.mk(OnTPat(Some(status)), ctx, u_gen, cursor_term),
      ),
    )
  };
};
