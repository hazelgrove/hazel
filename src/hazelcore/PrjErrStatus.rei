module HoleReason: {
  [@deriving sexp]
  type t =
    | DoesNotAppear;
  // | NotValid; Not valid labels prevented at the action level
};

[@deriving sexp]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InPrjHole(HoleReason.t, MetaVar.t);
