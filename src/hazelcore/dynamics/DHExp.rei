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
  | EmptyHole(MetaVar.t, MetaVarInst.t, env)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, env, t)
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, env, ExpandingKeyword.t)
  | FreeVar(MetaVar.t, MetaVarInst.t, env, Var.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, env, string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | FixF(Var.t, HTyp.t, t)
  | Fun(DHPat.t, HTyp.t, t)
  | Ap(t, t)
  | ApBuiltin(string, list(t))
  | BoolLit(bool)
  | IntLit(int)
  | FloatLit(float)
  | BinBoolOp(BinBoolOp.t, t, t)
  | BinIntOp(BinIntOp.t, t, t)
  | BinFloatOp(BinFloatOp.t, t, t)
  | ListNil(HTyp.t)
  | Cons(t, t)
  | Inj(inj)
  | InjError(InjErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, env, inj)
  | Pair(t, t)
  | Triv
  | ConsistentMatch(match, option(int))
  | InconsistentBranches(MetaVar.t, MetaVarInst.t, env, match, option(int))
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and inj = (HTyp.sum_body, UHTag.t, option(t))
and match =
  | Match(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t)
/* same as Environment.t, reiterated here to avoid circularity */
and env = VarMap.t_(t);

let constructor_string: t => string;

let mk_tuple: list(t) => t;

let cast: (t, HTyp.t, HTyp.t) => t;

let apply_casts: (t, list((HTyp.t, HTyp.t))) => t;
