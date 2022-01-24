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
  | EmptyHole(MetaVar.t, HoleClosureId.t, evalenv)
  | NonEmptyHole(
      ErrStatus.HoleReason.t,
      MetaVar.t,
      HoleClosureId.t,
      evalenv,
      t,
    )
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, HoleClosureId.t, evalenv, ExpandingKeyword.t)
  | FreeVar(MetaVar.t, HoleClosureId.t, evalenv, Var.t)
  | InvalidText(MetaVar.t, HoleClosureId.t, evalenv, string)
  | BoundVar(Var.t)
  | Let(DHPat.t, t, t)
  | Lam(DHPat.t, HTyp.t, t)
  | Closure(evalenv, DHPat.t, HTyp.t, t)
  | Ap(t, t)
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
  | InconsistentBranches(MetaVar.t, HoleClosureId.t, evalenv, case)
  | Cast(t, HTyp.t, HTyp.t)
  | FailedCast(t, HTyp.t, HTyp.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, t)
and environment = VarMap.t_(t)
and evalenv =
  | Env(int, VarMap.t_(result))
  | UnreachedEnv
and result =
  | BoxedValue(t)
  | Indet(t);

let constructor_string: t => string;

let mk_tuple: list(t) => t;

let cast: (t, HTyp.t, HTyp.t) => t;

let apply_casts: (t, list((HTyp.t, HTyp.t))) => t;

/* Used for faster structural equality checking. Structural
   checking may be slow when an expression is large,
   in particular when environments are repeated many times.
   We can optimize checking for structural equality of
   environments simply by checking equality of environment ID's.

   Note: assumes that environments with the same EvalEnvId.t
   within both expressions are equivalent. This assumption
   is true if comparing within a program evaluation (since
   EvalEnvId.t numbers don't get reused within a single program
   evaluation) or if all the environments are checked to be
   equal (see Result.fast_equals).
   */
let fast_equals: (t, t) => bool;
