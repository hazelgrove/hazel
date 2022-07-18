open Sexplib.Std;

[@deriving sexp]
type t =
  | EmptyHole
  | TyVar(TPatErrStatus.t, TyVar.t)
  | InvalidText(MetaVar.t, string);

let of_string = (text: string): t => TyVar(NotInHole, text);

let invalid_of_string = (id_gen: IDGen.t, t: TyVar.t): (t, IDGen.t) => {
  let (u, id_gen) = IDGen.next_hole(id_gen);
  let ty = InvalidText(u, t);
  (ty, id_gen);
};

let tyvar_name: t => option(TyVar.t) =
  fun
  | EmptyHole
  | InvalidText(_)
  | TyVar(InHole(_), _) => None
  | TyVar(NotInHole, t) => Some(t);

let is_complete =
  fun
  | EmptyHole
  | InvalidText(_)
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, _) => true;

let binds_tyvar = (t: TyVar.t): (t => bool) =>
  fun
  | EmptyHole
  | InvalidText(_)
  | TyVar(InHole(_), _) => false
  | TyVar(NotInHole, t') => TyVar.equal(t, t');
