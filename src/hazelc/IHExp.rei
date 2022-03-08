module BinBoolOp: {
  [@deriving sexp]
  type t =
    | And
    | Or;
};

module BinIntOp: {
  [@deriving sexp]
  type t =
    | Minus
    | Plus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;
};

module BinFloatOp: {
  [@deriving sexp]
  type t =
    | FPlus
    | FMinus
    | FTimes
    | FDivide
    | FLessThan
    | FGreaterThan
    | FEquals;
};

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t_(t))
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t_(t),
      t,
    )
  | Keyword(MetaVar.t, MetaVarInst.t, VarMap.t_(t), ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t_(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t_(t), string)
  | BoundVar(Var.t)
  | Let(IHPat.t, t, t)
  | LetRec(Var.t, HTyp.t, IHPat.t, t, t)
  | Lam(IHPat.t, HTyp.t, t)
  | Ap(t, t)
  | ApBuiltin(Var.t, list(t))
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(HTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | ConsistentCase(case)
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(IHPat.t, t);
