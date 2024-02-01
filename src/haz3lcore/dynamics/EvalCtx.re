open Sexplib.Std;
open DH;

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
