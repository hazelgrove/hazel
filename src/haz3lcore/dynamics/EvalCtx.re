open Sexplib.Std;
open DH;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Mark
  | Closure
  | FilterPattern
  | Filter
  | Seq1
  | Seq2
  | Let1
  | Let2
  | Ap1
  | Ap2
  | Fun
  | FixF
  | BinOp1
  | BinOp2
  | If1
  | If2
  | If3
  | Tuple(int)
  | ListLit(int)
  | ApBuiltin
  | Test
  | Cons1
  | Cons2
  | ListConcat1
  | ListConcat2
  | Prj
  | NonEmptyHole
  | Cast
  | FailedCast
  | InvalidOperation
  | MatchScrut
  | MatchRule(int)
  | FailedCastCast
  // Used when entering a bound variable expression in substitution mode
  | BoundVar;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Mark
  | Closure([@show.opaque] ClosureEnvironment.t, t)
  | Filter(DH.DHFilter.t, t)
  | Seq1(t, DHExp.t)
  | Seq2(DHExp.t, t)
  | Let1(DHPat.t, t, DHExp.t)
  | Let2(DHPat.t, DHExp.t, t)
  | Fun(DHPat.t, Typ.t, t, option(ClosureEnvironment.t), option(Var.t))
  | FixF(Var.t, Typ.t, t)
  | Ap1(t, DHExp.t)
  | Ap2(DHExp.t, t)
  | If1(consistency, t, DHExp.t, DHExp.t)
  | If2(consistency, DHExp.t, t, DHExp.t)
  | If3(consistency, DHExp.t, DHExp.t, t)
  | BinOp1(TermBase.UExp.op_bin, t, DHExp.t)
  | BinOp2(TermBase.UExp.op_bin, DHExp.t, t)
  | Tuple(t, (list(DHExp.t), list(DHExp.t)))
  | ApBuiltin(string, t)
  | Test(KeywordID.t, t)
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
  | MatchScrut(DH.consistency, t, list((DHPat.t, DHExp.t)))
  | MatchRule(
      DH.consistency,
      DHExp.t,
      DHPat.t,
      t,
      (list((DHPat.t, DHExp.t)), list((DHPat.t, DHExp.t))),
    );

let rec fuzzy_mark =
  fun
  | Mark => true
  | Closure(_, x)
  | Test(_, x)
  | Cast(x, _, _)
  | FailedCast(x, _, _)
  | Filter(_, x) => fuzzy_mark(x)
  | Seq1(_)
  | Seq2(_)
  | Let1(_)
  | Let2(_)
  | Fun(_)
  | FixF(_)
  | Ap1(_)
  | Ap2(_)
  | If1(_)
  | If2(_)
  | If3(_)
  | BinOp1(_)
  | BinOp2(_)
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
  | MatchScrut(_)
  | MatchRule(_) => false;

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
  | (Seq1, Seq1(c, _))
  | (Seq2, Seq2(_, c))
  | (Let1, Let1(_, c, _))
  | (Let2, Let2(_, _, c))
  | (Fun, Fun(_, _, c, _, _))
  | (FixF, FixF(_, _, c))
  | (Ap1, Ap1(c, _))
  | (Ap2, Ap2(_, c))
  | (BinOp1, BinOp1(_, c, _))
  | (BinOp2, BinOp2(_, _, c))
  | (If1, If1(_, c, _, _))
  | (If2, If2(_, _, c, _))
  | (If3, If3(_, _, _, c))
  | (Cons1, Cons1(c, _))
  | (Cons2, Cons2(_, c))
  | (ListConcat1, ListConcat1(c, _))
  | (ListConcat2, ListConcat2(_, c))
  | (Test, Test(_, c))
  | (Prj, Prj(c, _)) => Some(c)
  | (ListLit(n), ListLit(_, _, _, c, (ld, _)))
  | (Tuple(n), Tuple(c, (ld, _))) =>
    if (List.length(ld) == n) {
      Some(c);
    } else {
      None;
    }
  | (MatchScrut, MatchScrut(_, scr, _)) => Some(scr)
  | (MatchRule(n), MatchRule(_, _, _, c, (ld, _))) =>
    if (List.length(ld) == n) {
      Some(c);
    } else {
      None;
    }
  | (Cast, Cast(c, _, _))
  | (FailedCastCast, FailedCast(Cast(c, _, _), _, _))
  | (FailedCast, FailedCast(c, _, _)) => Some(c)
  | (Ap1, Ap2(_, _))
  | (Ap2, Ap1(_, _))
  | (If1, If2(_))
  | (If1, If3(_))
  | (If2, If1(_))
  | (If2, If3(_))
  | (If3, If1(_))
  | (If3, If2(_))
  | (Let1, Let2(_))
  | (Let2, Let1(_))
  | (BinOp1, BinOp2(_))
  | (BinOp2, BinOp1(_))
  | (Cons1, Cons2(_))
  | (Cons2, Cons1(_))
  | (Seq1, Seq2(_))
  | (Seq2, Seq1(_))
  | (ListConcat1, ListConcat2(_))
  | (ListConcat2, ListConcat1(_)) => None
  | (FilterPattern, _) => None
  | (MatchScrut, MatchRule(_))
  | (MatchRule(_), MatchScrut(_)) => None
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
