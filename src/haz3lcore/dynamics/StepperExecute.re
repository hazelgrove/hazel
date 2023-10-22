type t =
  | Result(StepperResult.t(DHExp.t))
  | Continue(closure)
  | Builtin(string, list(DHExp.t))
  | BinBoolOp(TermBase.UExp.op_bin_bool, bool, closure)
  | BinIntOp(TermBase.UExp.op_bin_int, int, int)
  | BinFloatOp(TermBase.UExp.op_bin_float, float, float)
  | BinStringOp(TermBase.UExp.op_bin_string, string, string)
  | ListConcat(
      MetaVar.t,
      MetaVarInst.t,
      Typ.t,
      list(DHExp.t),
      list(DHExp.t),
    )
  | Prj(list(DHExp.t), int)
  | Cons(MetaVar.t, MetaVarInst.t, Typ.t, DHExp.t, list(DHExp.t))
and closure =
  | Closure(
      [@opaque] ClosureEnvironment.t,
      [@opaque] FilterEnvironment.t,
      DHExp.t,
    );
