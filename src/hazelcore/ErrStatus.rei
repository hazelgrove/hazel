module HoleReason: {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent
    | WrongLength
    | OperatorError(OperatorErrStatus.HoleReason.t);

  let eq: (t, t) => bool;
};

/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);

/* creates an InHole error status, recycling the metavar from the given err status if available */
let make_recycled_InHole:
  (t, HoleReason.t, MetaVarGen.t) => (t, MetaVarGen.t);
