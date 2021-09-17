[@deriving sexp]
type t =
  | HoleLabel
  | Delim
  | EmptyTagHole(MetaVar.t)
  | NonEmptyTagHole(TagErrStatus.HoleReason.t, MetaVar.t);
