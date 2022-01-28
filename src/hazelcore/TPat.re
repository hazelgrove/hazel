/* type variable pattern errors */
module HoleReason = {
  // [@deriving sexp]
  type t =
    | Reserved
    | InvalidName;
};

/* type variable pattern hole status */
module Status = {
  // [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

// [@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, TyVar.Name.t);

let is_complete =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, _) => true;

let of_name = (name: TyVar.Name.t): t => TyVar(NotInHole, name);

let binds_tyvar = (name: TyVar.Name.t): (t => bool) =>
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, name') => TyVar.Name.equal(name, name');
