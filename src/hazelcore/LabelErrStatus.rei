module HoleReason = {
  [@deriving sexp]
  type t =
    | Standalone
    | Duplicate
};

[@deriving sexp]
type t =
  | NotInLabelHole
  | InLabelHole(HoleReason.t, MetaVar.t);