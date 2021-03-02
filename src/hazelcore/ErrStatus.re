open Sexplib.Std;

module SubReason = {
  [@deriving sexp]
  type t =
    | InsufficientParams
    | DoesNotExpand // TODO: rename to DecodingError
    | IllTypedExpansion
};

module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent(option(SubReason.t))
    | WrongLength;
};

/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
