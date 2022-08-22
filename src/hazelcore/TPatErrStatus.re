/* type variable pattern errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | ReservedKeyword
    | BuiltinType;
};

[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
