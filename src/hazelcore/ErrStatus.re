open Sexplib.Std;

module SubReason = {
  [@deriving sexp]
  type t =
    | InsufficientParams
    | DoesNotExpand;
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
