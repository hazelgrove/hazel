/* type variable pattern errors */
module HoleReason: {
  // [@deriving sexp]
  type t =
    | Reserved
    | InvalidName;
};

/* type variable pattern hole status */
module Status: {
  // [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

// [@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, TyVar.Name.t);

let binds_tyvar: (TyVar.Name.t, t) => bool;
