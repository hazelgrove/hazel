open Sexplib.Std;

open Holes;

module Label = PatLabel;

[@deriving sexp]
type t = {
  kind,
  label: PatLabel.t,
}

[@deriving sexp]
and kind =
  /* Holes */
  | PEmptyHole(MetaVar.t, MetaVarInst.t)
  | PNonEmptyHole(HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | PKeyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | PInvalidText(MetaVar.t, MetaVarInst.t, string)
  /* Non-holes */
  | PAp(t, t)
  | PPair(t, t)
  | PCons(t, t)
  | PInj(inj_side, t)
  | PWild
  | PVar(Ident.t)
  | PIntLit(int)
  | PFloatLit(float)
  | PBoolLit(bool)
  | PNil
  | PTriv

[@deriving sexp]
and inj_side =
  | L
  | R;
