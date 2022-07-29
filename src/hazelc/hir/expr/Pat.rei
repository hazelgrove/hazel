[@deriving sexp]
type t = {kind}

[@deriving sexp]
and kind =
  /* Holes */
  | PEmptyHole(MetaVar.t, MetaVarInst.t)
  | PNonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | PKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | PInvalidText(MetaVar.t, MetaVarInst.t, string)
  /* Non-holes */
  | PAp(t, t)
  | PPair(t, t)
  | PCons(t, t)
  | PInj(InjSide.t, t)
  | PWild
  | PVar(Var.t)
  | PIntLit(int)
  | PFloatLit(float)
  | PBoolLit(bool)
  | PNil
  | PTriv;
