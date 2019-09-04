module InHoleReason = {
  /* Variables: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent
    | WrongLength;
};

/* Variables: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(InHoleReason.t, MetaVar.t);
