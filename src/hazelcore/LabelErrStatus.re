module HoleReason = {
  [@deriving sexp]
  type t =
    | Standalone
    | Duplicate
    | Empty
    | Invalid
};

[@deriving sexp]
type t =
  | NotInLabelHole
  | InLabelHole(HoleReason.t, MetaVar.t);