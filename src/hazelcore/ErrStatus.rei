module SubReason: {
  [@deriving sexp]
  type t =
    | InsufficientParams
    | DecodingError
    | IllTypedExpansion;
};

module HoleReason: {
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
