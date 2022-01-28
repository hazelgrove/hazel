let fix_holes:
  (TyCtx.t, HTyp.t, ~renumber_empty_holes: bool, MetaVarGen.t) =>
  (HTyp.t, MetaVarGen.t);
