module EvalCtx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cls =
    | Mark
    | Closure
    | Filter
    | Sequence
    | Let
    | Ap1
    | Ap2
    | BinBoolOp1
    | BinBoolOp2
    | BinIntOp1
    | BinIntOp2
    | BinFloatOp1
    | BinFloatOp2
    | BinStringOp1
    | BinStringOp2
    | Tuple(int)
    | ListLit(int)
    | Cons1
    | Cons2
    | Prj
    | Inj
    | NonEmptyHole
    | Cast
    | FailedCast
    | InvalidOperation
    | ConsistentCase
    | InconsistentBranches;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Mark
    | Closure(ClosureEnvironment.t, t)
    | Filter(list((DHExp.t, TermBase.UExp.filter_action)), t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | BinStringOp1(DHExp.BinStringOp.t, t, DHExp.t)
    | BinStringOp2(DHExp.BinStringOp.t, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        ListErrStatus.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Prj(t, int)
    | Inj(Typ.t, InjSide.t, t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | ConsistentCase(case)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  and case =
    | Case(t, list(rule), int)
  and rule = DHExp.rule;
};

module EvalFilter: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = TermBase.UExp.filter_action;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list((DHExp.t, action));

  let init: t;

  let matches_exp: (DHExp.t, DHExp.t) => bool;
  let matches_pat: (DHPat.t, DHPat.t) => bool;
  let matches_typ: (Typ.t, Typ.t) => bool;
  let matches_rul: (DHExp.rule, DHExp.rule) => bool;
};

module EvalObj: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    /* env: ClosureEnvironment.t, */
    ctx: EvalCtx.t,
    exp: DHExp.t,
    /* flt: EvalFilter.t, */
  };

  /* let mk: (ClosureEnvironment.t, EvalCtx.t, DHExp.t, EvalFilter.t) => t; */
  let mk: (EvalCtx.t, DHExp.t) => t;

  let init: DHExp.t => t;

  /* let get_env: t => ClosureEnvironment.t; */
  let get_ctx: t => EvalCtx.t;
  let get_exp: t => DHExp.t;

  let unwrap: (t, EvalCtx.cls) => option(t);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t);

let step: EvalObj.t => (EvaluatorState.t, t);

let decompose: DHExp.t => (EvaluatorState.t, list(EvalObj.t));
