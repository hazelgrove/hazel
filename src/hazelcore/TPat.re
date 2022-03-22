open Sexplib.Std;

/* type variable pattern errors */
module HoleReason = {
  [@deriving sexp]
  type t =
    | ReservedKeyword
    | BuiltinType
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

let is_complete =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, _) => true;

let of_string = (name: string): t => TyVar(NotInHole, name);

let binds_tyvar = (name: string): (t => bool) =>
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, name') => String.equal(name, name');

let tyvar_name: t => option(string) =
  fun
  | EmptyHole
  | TyVar(InHole(_), _) => None
  | TyVar(NotInHole, name) => Some(name);
