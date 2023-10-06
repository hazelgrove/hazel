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
    | ListConcat1
    | ListConcat2
    | Prj
    | NonEmptyHole
    | Cast
    | FailedCast
    | InvalidOperation
    | ConsistentCase
    | InconsistentBranches;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Mark
    | Closure(
        [@opaque] ClosureEnvironment.t,
        [@opaque] InstrumentEnvironment.t,
        t,
      )
    | Instrument(Instrument.t, t)
    | Sequence(t, DHExp.t)
    | Let(DHPat.t, t, DHExp.t)
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(TermBase.UExp.op_bin_bool, t, DHExp.t)
    | BinBoolOp2(TermBase.UExp.op_bin_bool, DHExp.t, t)
    | BinIntOp1(TermBase.UExp.op_bin_int, t, DHExp.t)
    | BinIntOp2(TermBase.UExp.op_bin_int, DHExp.t, t)
    | BinFloatOp1(TermBase.UExp.op_bin_float, t, DHExp.t)
    | BinFloatOp2(TermBase.UExp.op_bin_float, DHExp.t, t)
    | BinStringOp1(TermBase.UExp.op_bin_string, t, DHExp.t)
    | BinStringOp2(TermBase.UExp.op_bin_string, DHExp.t, t)
    | Tuple(t, (list(DHExp.t), list(DHExp.t)))
    | ListLit(
        MetaVar.t,
        MetaVarInst.t,
        Typ.t,
        t,
        (list(DHExp.t), list(DHExp.t)),
      )
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | ListConcat1(t, DHExp.t)
    | ListConcat2(DHExp.t, t)
    | Prj(t, int)
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

module EvalObj: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    env: ClosureEnvironment.t,
    ins: InstrumentEnvironment.t,
    ctx: EvalCtx.t,
    exp: DHExp.t,
  };

  let mk:
    (ClosureEnvironment.t, InstrumentEnvironment.t, EvalCtx.t, DHExp.t) => t;

  let get_ctx: t => EvalCtx.t;
  let get_exp: t => DHExp.t;

  let unwrap: (t, EvalCtx.cls) => option(t);
};

let evaluate_with_history: DHExp.t => list(DHExp.t);

let step: EvalObj.t => (EvaluatorState.t, EvaluatorResult.t);

let decompose: DHExp.t => (EvaluatorState.t, list(EvalObj.t));
