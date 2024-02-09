open Sexplib.Std;
open Util;
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

let rec compose = (ctx: t, d: DHExp.t): DHExp.t => {
  DHExp.(
    switch (ctx) {
    | Mark => d
    | Closure(env, ctx) =>
      let d = compose(ctx, d);
      Closure(env, d) |> fresh;
    | Filter(flt, ctx) =>
      let d = compose(ctx, d);
      Filter(flt, d) |> fresh;
    | Seq1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Seq(d1, d2) |> fresh;
    | Seq2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Seq(d1, d2) |> fresh;
    | Ap1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Ap(d1, d2) |> fresh;
    | Ap2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Ap(d1, d2) |> fresh;
    | ApBuiltin(s, ctx) =>
      let d' = compose(ctx, d);
      ApBuiltin(s, d') |> fresh;
    | If1(c, ctx, d2, d3) =>
      let d' = compose(ctx, d);
      If(c, d', d2, d3) |> fresh;
    | If2(c, d1, ctx, d3) =>
      let d' = compose(ctx, d);
      If(c, d1, d', d3) |> fresh;
    | If3(c, d1, d2, ctx) =>
      let d' = compose(ctx, d);
      If(c, d1, d2, d') |> fresh;
    | Test(lit, ctx) =>
      let d1 = compose(ctx, d);
      Test(lit, d1) |> fresh;
    | BinOp1(op, ctx, d2) =>
      let d1 = compose(ctx, d);
      BinOp(op, d1, d2) |> fresh;
    | BinOp2(op, d1, ctx) =>
      let d2 = compose(ctx, d);
      BinOp(op, d1, d2) |> fresh;
    | Cons1(ctx, d2) =>
      let d1 = compose(ctx, d);
      Cons(d1, d2) |> fresh;
    | Cons2(d1, ctx) =>
      let d2 = compose(ctx, d);
      Cons(d1, d2) |> fresh;
    | ListConcat1(ctx, d2) =>
      let d1 = compose(ctx, d);
      ListConcat(d1, d2) |> fresh;
    | ListConcat2(d1, ctx) =>
      let d2 = compose(ctx, d);
      ListConcat(d1, d2) |> fresh;
    | Tuple(ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      Tuple(ListUtil.rev_concat(ld, [d, ...rd])) |> fresh;
    | ListLit(m, i, t, ctx, (ld, rd)) =>
      let d = compose(ctx, d);
      ListLit(m, i, t, ListUtil.rev_concat(ld, [d, ...rd])) |> fresh;
    | Let1(dp, ctx, d2) =>
      let d = compose(ctx, d);
      Let(dp, d, d2) |> fresh;
    | Let2(dp, d1, ctx) =>
      let d = compose(ctx, d);
      Let(dp, d1, d) |> fresh;
    | Fun(dp, t, ctx, env, v) =>
      let d = compose(ctx, d);
      Fun(dp, t, d, env, v) |> fresh;
    | FixF(v, t, ctx) =>
      let d = compose(ctx, d);
      FixF(v, t, d) |> fresh;
    | Prj(ctx, n) =>
      let d = compose(ctx, d);
      Prj(d, n) |> fresh;
    | Cast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      Cast(d, ty1, ty2) |> fresh;
    | FailedCast(ctx, ty1, ty2) =>
      let d = compose(ctx, d);
      FailedCast(d, ty1, ty2) |> fresh;
    | InvalidOperation(ctx, err) =>
      let d = compose(ctx, d);
      InvalidOperation(d, err) |> fresh;
    | NonEmptyHole(reason, u, i, ctx) =>
      let d = compose(ctx, d);
      NonEmptyHole(reason, u, i, d) |> fresh;
    | MatchScrut(c, ctx, rules) =>
      let d = compose(ctx, d);
      Match(c, d, rules) |> fresh;
    | MatchRule(c, scr, p, ctx, (lr, rr)) =>
      let d = compose(ctx, d);
      Match(c, scr, ListUtil.rev_concat(lr, [(p, d), ...rr])) |> fresh;
    }
  );
};
