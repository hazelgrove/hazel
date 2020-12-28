module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent
    | WrongLength
    | OperatorError(VarErrStatus.t);
};

/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
