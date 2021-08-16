let rec trans_DHExp = (d: DHExp.t): CHExp.t => {
  switch (d) {
  | EmptyHole(metavar, metavarinst, varmap) =>
    Meta(metavar, metavarinst, trans_VarMap(varmap), EmptyHole)
  | NonEmptyHole(reason, metavar, metavarinst, varmap, d) =>
    Meta(
      metavar,
      metavarinst,
      trans_VarMap(varmap),
      NonEmptyHole(reason, trans_DHExp(d)),
    )
  | Keyword(metavar, metavarinst, varmap, key) =>
    Meta(metavar, metavarinst, trans_VarMap(varmap), Keyword(key))
  | FreeVar(metavar, metavarinst, varmap, var) =>
    Meta(metavar, metavarinst, trans_VarMap(varmap), FreeVar(var))
  | InvalidText(metavar, metavarinst, varmap, string) =>
    Meta(metavar, metavarinst, trans_VarMap(varmap), InvalidText(string))
  | BoundVar(v) => BoundVar(v)
  | Let(dp, d1, d2) =>
    Ap(trans_CHExp(CHExp.Lam(dp, None, trans_DHExp(d2))), trans_DHExp(d1))
  | FixF(var, ty, d1) => FixF(var, Some(ty), trans_DHExp(d1))
  | Lam(dp, ty, d1) => trans_CHExp(Lam(dp, Some(ty), trans_DHExp(d1)))
  | Ap(d1, d2) => Ap(trans_DHExp(d1), trans_DHExp(d2))
  | BoolLit(b) => BoolLit(b)
  | IntLit(i) => IntLit(i)
  | FloatLit(f) => FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    BinBoolOp(op, trans_DHExp(d1), trans_DHExp(d2))
  | BinIntOp(op, d1, d2) => BinIntOp(op, trans_DHExp(d1), trans_DHExp(d2))
  | BinFloatOp(op, d1, d2) =>
    BinFloatOp(op, trans_DHExp(d1), trans_DHExp(d2))
  | ListNil(ty) => ListNil(ty)
  | Cons(d1, d2) => Cons(trans_DHExp(d1), trans_DHExp(d2))
  | Inj(ty, side, d) => Inj(ty, side, trans_DHExp(d))
  | Pair(d1, d2) => Pair(trans_DHExp(d1), trans_DHExp(d2))
  | Triv => Triv
  //   | ConsistentCase(case)
  //   | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | Cast(d, ty1, ty2) => Cast(trans_DHExp(d), ty1, ty2)
  | FailedCast(d, ty1, ty2) => FailedCast(trans_DHExp(d), ty1, ty2)
  | InvalidOperation(d, err) => InvalidOperation(trans_DHExp(d), err)
  | _ => BuiltIn(Indet)
  };
}
and trans_VarMap = (varmap: VarMap.t_(DHExp.t)): VarMap.t_(CHExp.t) => {
  List.map(((v, e)) => (v, trans_DHExp(e)), varmap);
}
and trans_CHExp = (d: CHExp.t): CHExp.t => {
  switch (d) {
  | _ => BuiltIn(Indet)
  };
};
