module HoleReason = {
  [@deriving sexp]
  type t =
    | Standalone
    | Duplicate
    | Empty;
  // | NotValid; Not valid labels prevented at the action level
};

[@deriving sexp]
type t =
  | NotInLabelHole
  | InLabelHole(HoleReason.t, MetaVar.t);
