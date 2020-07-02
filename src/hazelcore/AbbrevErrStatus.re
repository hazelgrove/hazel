module HoleReason = {
  [@deriving sexp]
  type t =
    | Free
    | ExtraneousArgs;
};

[@deriving sexp]
type t =
  | NotInAbbrevHole
  | InAbbrevHole(HoleReason.t, MetaVar.t);
