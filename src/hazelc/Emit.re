exception NotImplemented;

let emit = (d: IHExp.t) => {
  switch (d) {
  | IHExp.Keyword(_, _, _, _)
  | IHExp.FreeVar(_, _, _, _)
  | IHExp.InvalidText(_, _, _, _)
  | IHExp.BoundVar(_)
  | IHExp.Let(_, _, _)
  | IHExp.FixF(_, _, _)
  | IHExp.Lam(_, _, _)
  | IHExp.Ap(_, _)
  | IHExp.BoolLit(_)
  | IHExp.IntLit(_)
  | IHExp.FloatLit(_)
  | IHExp.BinBoolOp(_, _, _)
  | IHExp.BinIntOp(_, _, _)
  | IHExp.BinFloatOp(_, _, _)
  | IHExp.ListNil(_)
  | IHExp.Cons(_, _)
  | IHExp.Inj(_, _, _)
  | IHExp.Pair(_, _)
  | IHExp.Triv
  | IHExp.ConsistentCase(_) => raise(NotImplemented)
  | IHExp.EmptyHole(_, _, _)
  | IHExp.NonEmptyHole(_, _, _, _, _)
  | IHExp.InconsistentBranches(_, _, _, _)
  | IHExp.Cast(_, _, _)
  | IHExp.FailedCast(_, _, _)
  | IHExp.InvalidOperation(_, _) => raise(NotImplemented)
  };
};
