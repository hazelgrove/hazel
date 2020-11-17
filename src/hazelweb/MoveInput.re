[@deriving sexp]
type t =
  | Key(MoveKey.t)
  | Click(Pretty.MeasuredPosition.t);
