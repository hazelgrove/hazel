let rec trans_DHExp = (d: DHExp.t): CHExp.t => {
  switch (d) {
  // | EmptyHole(metavar, metavarinst, varmap) =>
  //   Meta(metavar, metavarinst, trans_VarMap(varmap), EmptyHole)
  // | NonEmptyHole(reason, metavar, metavarinst, varmap, d) =>
  //   Meta(
  //     metavar,
  //     metavarinst,
  //     trans_VarMap(varmap),
  //     NonEmptyHole(reason, trans_DHExp(d)),
  //   )
  // | Keyword(metavar, metavarinst, varmap, key) =>
  //   Meta(metavar, metavarinst, trans_VarMap(varmap), Keyword(key))
  // | FreeVar(metavar, metavarinst, varmap, var) =>
  //   Meta(metavar, metavarinst, trans_VarMap(varmap), FreeVar(var))
  // | InvalidText(metavar, metavarinst, varmap, string) =>
  //   Meta(metavar, metavarinst, trans_VarMap(varmap), InvalidText(string))
  // | BoundVar(v) => BoundVar(v)
  // | Let(dp, d1, d2) =>
  //   Ap(trans_Lam(CHExp.Lam(dp, None, trans_DHExp(d2))), trans_DHExp(d1))
  // | FixF(var, ty, d1) => FixF(var, Some(ty), trans_DHExp(d1))
  // | Lam(dp, ty, d1) => trans_Lam(Lam(dp, Some(ty), trans_DHExp(d1)))
  // | Ap(d1, d2) => Ap(trans_DHExp(d1), trans_DHExp(d2))
  | BoolLit(b) => BoolLit(b)
  | IntLit(i) => IntLit(i)
  | FloatLit(f) => FloatLit(f)
  | BinBoolOp(op, d1, d2) =>
    BinBoolOp(op, trans_DHExp(d1), trans_DHExp(d2))
  | BinIntOp(op, d1, d2) => BinIntOp(op, trans_DHExp(d1), trans_DHExp(d2))
  | BinFloatOp(op, d1, d2) =>
    BinFloatOp(op, trans_DHExp(d1), trans_DHExp(d2))
  // | ListNil(ty) => ListNil(ty)
  // | Cons(d1, d2) => Cons(trans_DHExp(d1), trans_DHExp(d2))
  // | Inj(ty, side, d) => Inj(ty, side, trans_DHExp(d))
  // | Pair(d1, d2) => Pair(trans_DHExp(d1), trans_DHExp(d2))
  // | Triv => Triv
  // | ConsistentCase(case)
  // | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  // | Cast(d, ty1, ty2) => Cast(trans_DHExp(d), ty1, ty2)
  // | FailedCast(d, ty1, ty2) => FailedCast(trans_DHExp(d), ty1, ty2)
  // | InvalidOperation(d, err) => InvalidOperation(trans_DHExp(d), err)
  | _ => BuiltIn(Indet)
  };
}
and trans_VarMap = (varmap: VarMap.t_(DHExp.t)): VarMap.t_(CHExp.t) => {
  List.map(((v, e)) => (v, trans_DHExp(e)), varmap);
}
and trans_Lam = (d: CHExp.t): CHExp.t => {
  switch (d) {
  | Lam(IntLit(x), _, d0) => BuiltIn(IfEqInt(x, d0, BuiltIn(Indet)))
  | Lam(FloatLit(x), _, d0) => BuiltIn(IfEqFloat(x, d0, BuiltIn(Indet)))
  | Lam(BoolLit(x), _, d0) => BuiltIn(IfEqBool(x, d0, BuiltIn(Indet)))
  | Lam(Pair(p1, p2), _, d0) =>
    BuiltIn(
      UnpackProd(
        2,
        trans_Lam(Lam(p1, None, trans_Lam(Lam(p2, None, d0)))),
      ),
    ) // None can be improved
  | Lam(Triv, _, d0) => BuiltIn(UnpackProd(1, d0))
  | Lam(Inj(side, p1), _, d0) =>
    BuiltIn(UnpackSum(side, trans_Lam(Lam(p1, None, d0))))
  | Lam(ListNil, _, d0) => BuiltIn(UnpackNil(d0))
  | Lam(Cons(p1, p2), _, d0) =>
    BuiltIn(
      UnpackCons(trans_Lam(Lam(p1, None, trans_Lam(Lam(p2, None, d0))))),
    )
  | d => d
  };
};

let rec gen_string = (n: int, d: CHExp.t): (int, string) => {
  switch (d) {
  | BoolLit(b) => (n, Printf.sprintf("%d BoolLit %B\n", n, b))
  | IntLit(i) => (n, Printf.sprintf("%d IntLit %d\n", n, i))
  | FloatLit(f) => (n, Printf.sprintf("%d IntLit %f\n", n, f)) //TODO precision
  | BinBoolOp(op, d1, d2) =>
    let (n1, s1) = gen_string(n, d1);
    let (n2, s2) = gen_string(n1 + 1, d2);
    let s0 =
      Printf.sprintf(
        "%d BinBoolOp %s %d %d\n",
        n2 + 1,
        DHExp.BinBoolOp.toString(op),
        n1,
        n2,
      );
    (n2 + 1, String.concat("", [s1, s2, s0]));
  // | BinIntOp(op, d1, d2)
  // | BinFloatOp(op, d1, d2)
  | _ => (n, "")
  };
};
