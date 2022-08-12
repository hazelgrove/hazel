module Label = PatLabel;

[@deriving sexp]
type t = {
  kind,
  label: PatLabel.t,
}

[@deriving sexp]
and kind =
  | PWild
  | PVar(Ident.t)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PInj(inj_side, t)
  | PCons(t, t)
  | PPair(t, t)
  | PTriv

[@deriving sexp]
and inj_side =
  | PInjL
  | PInjR;
