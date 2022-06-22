open Sexplib.Std;

/* type variable pattern errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | ReservedKeyword
    | BuiltinType
    /* TODO: (eric) rename InvalidName to InvalidText */
    | InvalidName;
};

/* type variable pattern hole status */
module Status = {
  [@deriving sexp]
  type t =
    | NotInHole
    | InHole(HoleReason.t, MetaVar.t);
};

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(Status.t, string);

let of_string = (t: string): t => TyVar(NotInHole, t);

let invalid_of_string = (u_gen: MetaVarGen.t, t: string): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  let ty = TyVar(InHole(InvalidName, u), t);
  (ty, u_gen);
};

let tyvar_name: t => option(string) =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => None
  | TyVar(NotInHole, name) => Some(name);

let is_complete =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, _) => true;

let binds_tyvar = (name: string): (t => bool) =>
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, name') => String.equal(name, name');
