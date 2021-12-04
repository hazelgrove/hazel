open Format;

exception NotImplemented;

let trans_expression = (d: DHExp.t, f: formatter):unit => {
  switch (d) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  // TODO rename to ExpandingKeyword
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_)
  | Cast(_)
  | FailedCast(_)
  | InvalidOperation(_) => raise(NotImplemented)
  | BoundVar(v) =>
    fprintf(f, "@s", v);
  | Ap(d1, d2) =>

  | BoolLit(b) =>

  | IntLit(i) =>

  | FloatLit(f) =>

  | BinBoolOp(op, d1, d2) =>

  | BinIntOp(op, d1, d2) =>

  | BinFloatOp(op, d1, d2) =>

  | Let(pat, d1, d2) =>

  | Lam(pat, _, d0) =>

  | Pair(d1, d2) =>

  | FixF(v, _, d0) =>

  | FixF(v, ty, d)
  | ListNil(ty)
  | Cons(d1, d2)
  | Inj(ty, side, d)
  | Triv
  | ConsistentCase(case) => raise(NotImplemented)
  };
};
