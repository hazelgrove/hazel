module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | InjectionInSyntheticPosition
    | ExpectedTypeNotConsistentWithSums
    | BadTag;

  let eq = (x, y) => x == y;
};

/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
