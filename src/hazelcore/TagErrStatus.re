module HoleReason = {
  [@deriving sexp]
  type t =
    | InvalidName
    | NotInSum
    | Duplicate;
};

[@deriving sexp]
type t =
  | NotInTagHole
  | InTagHole(HoleReason.t, MetaVar.t);
