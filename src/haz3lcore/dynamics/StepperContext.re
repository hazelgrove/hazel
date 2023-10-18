open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type cont =
  | Mark(DHExp.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
  | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
  | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
  | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  | Closure([@opaque] ClosureEnvironment.t, [@opaque] FilterEnvironment.t, t)
  | Sequence(t, DHExp.t)
  | Let(DHPat.t, t, DHExp.t)
  | Filter(Filter.t, t)
  | FixF(Var.t, Typ.t, t)
  | Ap(t, t)
  | ApBuiltin(string, list(t))
  | BinBoolOp1(TermBase.UExp.op_bin_bool, t)
  | BinIntOp1(TermBase.UExp.op_bin_int, t)
  | BinFloatOp1(TermBase.UExp.op_bin_float, t)
  | BinStringOp1(TermBase.UExp.op_bin_string, t)
  | ListLit(MetaVar.t, MetaVarInst.t, Typ.t, list(t))
  | Cons(t, t)
  | ListConcat1(t, t)
  | Tuple(list(t))
  | Prj(t, int)
  | ConsistentCase(case)
  | Cast(t, Typ.t, Typ.t)
  | FailedCast(t, Typ.t, Typ.t)
  | InvalidOperation(t, InvalidOperationError.t)
and case =
  | Case(t, list(rule), int)
and rule =
  | Rule(DHPat.t, DHExp.t)
and t =
  | Cont(cont)
  | Term(DHExp.t);

