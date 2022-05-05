module BinBoolOp: {
  [@deriving sexp]
  type t =
    | And
    | Or;

  let of_op: UHExp.operator => option(t);

  let to_op: t => UHExp.operator;
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

  let of_op: UHExp.operator => option((t, HTyp.t));

  let to_op: t => UHExp.operator;
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

  let of_op: UHExp.operator => option((t, HTyp.t));

  let to_op: t => UHExp.operator;
};

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t, VarMap.t(t))
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      MetaVarInst.t,
      VarMap.t(t),
      t,
    )
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, VarMap.t(t), ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, VarMap.t(t), Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, VarMap.t(t), string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | TyAlias(TPat.t, DHTyp.t, t)
  | FixF(Var.t, DHTyp.t, t)
  | Lam(DHPat.t, DHTyp.t, t)
  | Ap(t, t)
  | ApBuiltin(string, list(t))
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(DHTyp.t)
  | Cons(t, t)
  | Inj(DHTyp.t, InjSide.t, t)
  | Pair(t, t)
  | Triv
  | ConsistentCase(case)
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t(t), case)
  | Cast(t, DHTyp.t, DHTyp.t)
  | FailedCast(t, DHTyp.t, DHTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t);

let constructor_string: t => string;

let mk_tuple: list(t) => t;

let cast: (t, DHTyp.t, DHTyp.t) => t;

let apply_casts: (t, list((DHTyp.t, DHTyp.t))) => t;
