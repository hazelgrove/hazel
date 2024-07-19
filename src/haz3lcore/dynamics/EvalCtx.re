open Sexplib.Std;
open DH;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Mark
  | Closure
  | FilterPattern
  | Filter
  | Sequence1
  | Sequence2
  | Let1
  | Let2
  | TypAp
  | Ap1
  | Ap2
  | Fun
  | FixF
  | BinBoolOp1
  | BinBoolOp2
  | BinIntOp1
  | BinIntOp2
  | BinFloatOp1
  | BinFloatOp2
  | BinStringOp1
  | BinStringOp2
  | IfThenElse1
  | IfThenElse2
  | IfThenElse3
  | Tuple(int)
  | ListLit(int)
  | ApBuiltin
  | Test
  | HintedTest
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
  | ConsistentCaseRule(int)
  | InconsistentBranches
  | InconsistentBranchesRule(int)
  | FailedCastCast
  // Used when entering a bound variable expression in substitution mode
  | BoundVar;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Mark
  | Closure([@show.opaque] ClosureEnvironment.t, t)
  | Filter(DH.DHFilter.t, t)
  | Sequence1(t, DHExp.t)
  | Sequence2(DHExp.t, t)
  | Let1(DHPat.t, t, DHExp.t)
  | Let2(DHPat.t, DHExp.t, t)
  | Fun(DHPat.t, Typ.t, t, option(Var.t))
  | FixF(Var.t, Typ.t, t)
  | TypAp(t, Typ.t)
  | Ap1(t, DHExp.t)
  | Ap2(DHExp.t, t)
  | IfThenElse1(if_consistency, t, DHExp.t, DHExp.t)
  | IfThenElse2(if_consistency, DHExp.t, t, DHExp.t)
  | IfThenElse3(if_consistency, DHExp.t, DHExp.t, t)
  | BinBoolOp1(TermBase.UExp.op_bin_bool, t, DHExp.t)
  | BinBoolOp2(TermBase.UExp.op_bin_bool, DHExp.t, t)
  | BinIntOp1(TermBase.UExp.op_bin_int, t, DHExp.t)
  | BinIntOp2(TermBase.UExp.op_bin_int, DHExp.t, t)
  | BinFloatOp1(TermBase.UExp.op_bin_float, t, DHExp.t)
  | BinFloatOp2(TermBase.UExp.op_bin_float, DHExp.t, t)
  | BinStringOp1(TermBase.UExp.op_bin_string, t, DHExp.t)
  | BinStringOp2(TermBase.UExp.op_bin_string, DHExp.t, t)
  | Tuple(t, (list(DHExp.t), list(DHExp.t)))
  | ApBuiltin(string, t)
  | Test(KeywordID.t, t)
  | HintedTest(KeywordID.t, t)
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
  | ConsistentCaseRule(
      DHExp.t,
      DHPat.t,
      t,
      (list(DHExp.rule), list(DHExp.rule)),
      int,
    )
  | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  | InconsistentBranchesRule(
      DHExp.t,
      MetaVar.t,
      HoleInstanceId.t,
      DHPat.t,
      t,
      (list(DHExp.rule), list(DHExp.rule)),
      int,
    )
and case =
  | Case(t, list(rule), int)
and rule = DHExp.rule;

let rec fuzzy_mark =
  fun
  | Mark => true
  | Closure(_, x)
  | Test(_, x)
  | HintedTest(_, x)
  | Cast(x, _, _)
  | FailedCast(x, _, _)
  | Filter(_, x) => fuzzy_mark(x)
  | Sequence1(_)
  | Sequence2(_)
  | Let1(_)
  | Let2(_)
  | Fun(_)
  | FixF(_)
  | TypAp(_)
  | Ap1(_)
  | Ap2(_)
  | IfThenElse1(_)
  | IfThenElse2(_)
  | IfThenElse3(_)
  | BinBoolOp1(_)
  | BinBoolOp2(_)
  | BinIntOp1(_)
  | BinIntOp2(_)
  | BinFloatOp1(_)
  | BinFloatOp2(_)
  | BinStringOp1(_)
  | BinStringOp2(_)
  | Tuple(_)
  | ApBuiltin(_)
  | ListLit(_)
  | Cons1(_)
  | Cons2(_)
  | ListConcat1(_)
  | ListConcat2(_)
  | Prj(_)
  | NonEmptyHole(_)
  | InvalidOperation(_)
  | ConsistentCase(_)
  | ConsistentCaseRule(_)
  | InconsistentBranches(_)
  | InconsistentBranchesRule(_) => false;

let rec unwrap = (ctx: t, sel: cls): option(t) => {
  switch (sel, ctx) {
  | (Mark, _) =>
    print_endline(
      "Mark does not match with "
      ++ Sexplib.Sexp.to_string_hum(sexp_of_t(ctx)),
    );
    raise(EvaluatorError.Exception(StepDoesNotMatch));
  | (BoundVar, c)
  | (NonEmptyHole, NonEmptyHole(_, _, _, c))
  | (Closure, Closure(_, c))
  | (Filter, Filter(_, c))
  | (Sequence1, Sequence1(c, _))
  | (Sequence2, Sequence2(_, c))
  | (Let1, Let1(_, c, _))
  | (Let2, Let2(_, _, c))
  | (Fun, Fun(_, _, c, _))
  | (FixF, FixF(_, _, c))
  | (TypAp, TypAp(c, _))
  | (Ap1, Ap1(c, _))
  | (Ap2, Ap2(_, c))
  | (BinBoolOp1, BinBoolOp1(_, c, _))
  | (BinBoolOp2, BinBoolOp2(_, _, c))
  | (BinIntOp1, BinIntOp1(_, c, _))
  | (BinIntOp2, BinIntOp2(_, _, c))
  | (BinFloatOp1, BinFloatOp1(_, c, _))
  | (BinFloatOp2, BinFloatOp2(_, _, c))
  | (BinStringOp1, BinStringOp1(_, c, _))
  | (BinStringOp2, BinStringOp2(_, _, c))
  | (IfThenElse1, IfThenElse1(_, c, _, _))
  | (IfThenElse2, IfThenElse2(_, _, c, _))
  | (IfThenElse3, IfThenElse3(_, _, _, c))
  | (Cons1, Cons1(c, _))
  | (Cons2, Cons2(_, c))
  | (ListConcat1, ListConcat1(c, _))
  | (ListConcat2, ListConcat2(_, c))
  | (Test, Test(_, c))
  | (HintedTest, HintedTest(_, c))
  | (Prj, Prj(c, _)) => Some(c)
  | (ListLit(n), ListLit(_, _, _, c, (ld, _)))
  | (Tuple(n), Tuple(c, (ld, _))) =>
    if (List.length(ld) == n) {
      Some(c);
    } else {
      None;
    }
  | (ConsistentCaseRule(n), ConsistentCaseRule(_, _, c, (ld, _), _))
  | (
      InconsistentBranchesRule(n),
      InconsistentBranchesRule(_, _, _, _, c, (ld, _), _),
    ) =>
    if (List.length(ld) == n) {
      Some(c);
    } else {
      None;
    }
  | (InconsistentBranches, InconsistentBranches(_, _, Case(scrut, _, _))) =>
    Some(scrut)
  | (ConsistentCase, ConsistentCase(Case(scrut, _, _))) => Some(scrut)
  | (Cast, Cast(c, _, _))
  | (FailedCastCast, FailedCast(Cast(c, _, _), _, _))
  | (FailedCast, FailedCast(c, _, _)) => Some(c)
  | (Ap1, Ap2(_, _))
  | (Ap2, Ap1(_, _))
  | (IfThenElse1, IfThenElse2(_))
  | (IfThenElse1, IfThenElse3(_))
  | (IfThenElse2, IfThenElse1(_))
  | (IfThenElse2, IfThenElse3(_))
  | (IfThenElse3, IfThenElse1(_))
  | (IfThenElse3, IfThenElse2(_))
  | (Let1, Let2(_))
  | (Let2, Let1(_))
  | (BinBoolOp1, BinBoolOp2(_))
  | (BinBoolOp2, BinBoolOp1(_))
  | (BinIntOp1, BinIntOp2(_))
  | (BinIntOp2, BinIntOp1(_))
  | (BinFloatOp1, BinFloatOp2(_))
  | (BinFloatOp2, BinFloatOp1(_))
  | (BinStringOp1, BinStringOp2(_))
  | (BinStringOp2, BinStringOp1(_))
  | (Cons1, Cons2(_))
  | (Cons2, Cons1(_))
  | (Sequence1, Sequence2(_))
  | (Sequence2, Sequence1(_))
  | (ListConcat1, ListConcat2(_))
  | (ListConcat2, ListConcat1(_)) => None
  | (FilterPattern, _) => None
  | (Filter, _) => Some(ctx)
  | (tag, Filter(_, c)) => unwrap(c, tag)
  | (Closure, _) => Some(ctx)
  | (tag, Closure(_, c)) => unwrap(c, tag)
  | (Cast, _) => Some(ctx)
  | (tag, Cast(c, _, _)) => unwrap(c, tag)
  | (_, Mark) => None
  | (_, _) =>
    // print_endline(
    //   Sexplib.Sexp.to_string_hum(sexp_of_cls(tag))
    //   ++ " does not match with "
    //   ++ Sexplib.Sexp.to_string_hum(sexp_of_t(ctx)),
    // );
    None
  // raise(EvaluatorError.Exception(StepDoesNotMatch));
  };
};
