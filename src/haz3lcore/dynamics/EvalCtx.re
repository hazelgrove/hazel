open Sexplib.Std;
open Util;
open DH;

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Closure([@show.opaque] ClosureEnvironment.t, t)
  | Filter(DH.DHFilter.t, t)
  | Seq1(t, DHExp.t)
  | Seq2(DHExp.t, t)
  | Let1(DHPat.t, t, DHExp.t)
  | Let2(DHPat.t, DHExp.t, t)
  | Fun(DHPat.t, Typ.t, t, option(ClosureEnvironment.t), option(Var.t))
  | FixF(DHPat.t, Typ.t, t)
  | Ap1(TermBase.UExp.ap_direction, t, DHExp.t)
  | Ap2(TermBase.UExp.ap_direction, DHExp.t, t)
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
  | MultiHole(t, (list(DHExp.t), list(DHExp.t)))
  | Cons1(t, DHExp.t)
  | Cons2(DHExp.t, t)
  | ListConcat1(t, DHExp.t)
  | ListConcat2(DHExp.t, t)
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
    )
and t =
  | Mark
  | Term({
      term,
      ids: list(Id.t),
    });

let rec compose = (ctx: t, d: DHExp.t): DHExp.t => {
  switch (ctx) {
  | Mark => d
  | Term({term, ids}) =>
    let wrap = DHExp.mk(ids);
    DHExp.(
      switch (term) {
      | Closure(env, ctx) =>
        let d = compose(ctx, d);
        Closure(env, d) |> wrap;
      | Filter(flt, ctx) =>
        let d = compose(ctx, d);
        Filter(flt, d) |> wrap;
      | Seq1(ctx, d2) =>
        let d1 = compose(ctx, d);
        Seq(d1, d2) |> wrap;
      | Seq2(d1, ctx) =>
        let d2 = compose(ctx, d);
        Seq(d1, d2) |> wrap;
      | Ap1(dir, ctx, d2) =>
        let d1 = compose(ctx, d);
        Ap(dir, d1, d2) |> wrap;
      | Ap2(dir, d1, ctx) =>
        let d2 = compose(ctx, d);
        Ap(dir, d1, d2) |> wrap;
      | ApBuiltin(s, ctx) =>
        let d' = compose(ctx, d);
        ApBuiltin(s, d') |> wrap;
      | If1(c, ctx, d2, d3) =>
        let d' = compose(ctx, d);
        If(c, d', d2, d3) |> wrap;
      | If2(c, d1, ctx, d3) =>
        let d' = compose(ctx, d);
        If(c, d1, d', d3) |> wrap;
      | If3(c, d1, d2, ctx) =>
        let d' = compose(ctx, d);
        If(c, d1, d2, d') |> wrap;
      | Test(lit, ctx) =>
        let d1 = compose(ctx, d);
        Test(lit, d1) |> wrap;
      | BinOp1(op, ctx, d2) =>
        let d1 = compose(ctx, d);
        BinOp(op, d1, d2) |> wrap;
      | BinOp2(op, d1, ctx) =>
        let d2 = compose(ctx, d);
        BinOp(op, d1, d2) |> wrap;
      | Cons1(ctx, d2) =>
        let d1 = compose(ctx, d);
        Cons(d1, d2) |> wrap;
      | Cons2(d1, ctx) =>
        let d2 = compose(ctx, d);
        Cons(d1, d2) |> wrap;
      | ListConcat1(ctx, d2) =>
        let d1 = compose(ctx, d);
        ListConcat(d1, d2) |> wrap;
      | ListConcat2(d1, ctx) =>
        let d2 = compose(ctx, d);
        ListConcat(d1, d2) |> wrap;
      | Tuple(ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        Tuple(ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | ListLit(m, i, t, ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        ListLit(m, i, t, ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | MultiHole(ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        MultiHole(ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | Let1(dp, ctx, d2) =>
        let d = compose(ctx, d);
        Let(dp, d, d2) |> wrap;
      | Let2(dp, d1, ctx) =>
        let d = compose(ctx, d);
        Let(dp, d1, d) |> wrap;
      | Fun(dp, t, ctx, env, v) =>
        let d = compose(ctx, d);
        Fun(dp, t, d, env, v) |> wrap;
      | FixF(v, t, ctx) =>
        let d = compose(ctx, d);
        FixF(v, t, d) |> wrap;
      | Cast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        Cast(d, ty1, ty2) |> wrap;
      | FailedCast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        FailedCast(d, ty1, ty2) |> wrap;
      | InvalidOperation(ctx, err) =>
        let d = compose(ctx, d);
        InvalidOperation(d, err) |> wrap;
      | NonEmptyHole(reason, u, i, ctx) =>
        let d = compose(ctx, d);
        NonEmptyHole(reason, u, i, d) |> wrap;
      | MatchScrut(c, ctx, rules) =>
        let d = compose(ctx, d);
        Match(c, d, rules) |> wrap;
      | MatchRule(c, scr, p, ctx, (lr, rr)) =>
        let d = compose(ctx, d);
        Match(c, scr, ListUtil.rev_concat(lr, [(p, d), ...rr])) |> wrap;
      }
    );
  };
};
