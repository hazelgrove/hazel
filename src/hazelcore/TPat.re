[@deriving sexp]
type t = operand
and operand =
  | EmptyHole(MetaVar.t)
  | TyVar(VarErrStatus.t, TyId.t);

let is_complete =
  fun
  | EmptyHole(_) => false
  | TyVar(InVarHole(_), _) => false
  | TyVar(NotInVarHole, _) => true;

let new_EmptyHole = (u_gen: MetaVarGen.t): (operand, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (EmptyHole(u), u_gen);
};
