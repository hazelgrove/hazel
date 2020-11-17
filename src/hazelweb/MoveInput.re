open Sexplib.Std;

[@deriving sexp]
type t =
  | Key(MoveKey.t)
  | Click(option((MetaVar.t, SpliceName.t)), Pretty.MeasuredPosition.t);
