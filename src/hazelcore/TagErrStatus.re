module HoleReason = {
  [@deriving sexp]
  type t =
    | InvalidTagName
    | DuplicateTagName;
};

[@deriving sexp]
type t =
  | NotInTagHole
  | InTagHole(HoleReason.t, MetaVar.t);
