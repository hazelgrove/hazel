open Sexplib.Std;
open Util;
open DH;

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Closure([@show.opaque] ClosureEnvironment.t, t)
  | Filter(DH.DHFilter.t, t)
  | Seq1(t, DHExp.t)
  | Seq2(DHExp.t, t)
  | Let1(TermBase.UPat.t, t, DHExp.t)
  | Let2(TermBase.UPat.t, DHExp.t, t)
  | Fun(TermBase.UPat.t, t, option(ClosureEnvironment.t), option(Var.t))
  | FixF(TermBase.UPat.t, t)
  | Ap1(TermBase.UExp.ap_direction, t, DHExp.t)
  | Ap2(TermBase.UExp.ap_direction, DHExp.t, t)
  | If1(t, DHExp.t, DHExp.t)
  | If2(DHExp.t, t, DHExp.t)
  | If3(DHExp.t, DHExp.t, t)
  | UnOp(TermBase.UExp.op_un, t)
  | BinOp1(TermBase.UExp.op_bin, t, DHExp.t)
  | BinOp2(TermBase.UExp.op_bin, DHExp.t, t)
  | Tuple(t, (list(DHExp.t), list(DHExp.t)))
  | Test(t)
  | ListLit(Typ.t, t, (list(DHExp.t), list(DHExp.t)))
  | MultiHole(t, (list(DHExp.t), list(DHExp.t)))
  | Cons1(t, DHExp.t)
  | Cons2(DHExp.t, t)
  | ListConcat1(t, DHExp.t)
  | ListConcat2(DHExp.t, t)
  | StaticErrorHole(Id.t, t)
  | Cast(t, Typ.t, Typ.t)
  | FailedCast(t, Typ.t, Typ.t)
  | DynamicErrorHole(t, InvalidOperationError.t)
  | MatchScrut(t, list((TermBase.UPat.t, DHExp.t)))
  | MatchRule(
      DHExp.t,
      TermBase.UPat.t,
      t,
      (
        list((TermBase.UPat.t, DHExp.t)),
        list((TermBase.UPat.t, DHExp.t)),
      ),
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
      | If1(ctx, d2, d3) =>
        let d' = compose(ctx, d);
        If(d', d2, d3) |> wrap;
      | If2(d1, ctx, d3) =>
        let d' = compose(ctx, d);
        If(d1, d', d3) |> wrap;
      | If3(d1, d2, ctx) =>
        let d' = compose(ctx, d);
        If(d1, d2, d') |> wrap;
      | Test(ctx) =>
        let d1 = compose(ctx, d);
        Test(d1) |> wrap;
      | UnOp(op, ctx) =>
        let d1 = compose(ctx, d);
        UnOp(op, d1) |> wrap;
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
      | ListLit(t, ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        ListLit(t, ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | MultiHole(ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        MultiHole(ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | Let1(dp, ctx, d2) =>
        let d = compose(ctx, d);
        Let(dp, d, d2) |> wrap;
      | Let2(dp, d1, ctx) =>
        let d = compose(ctx, d);
        Let(dp, d1, d) |> wrap;
      | Fun(dp, ctx, env, v) =>
        let d = compose(ctx, d);
        Fun(dp, d, env, v) |> wrap;
      | FixF(v, ctx) =>
        let d = compose(ctx, d);
        FixF(v, d) |> wrap;
      | Cast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        Cast(d, ty1, ty2) |> wrap;
      | FailedCast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        FailedCast(d, ty1, ty2) |> wrap;
      | DynamicErrorHole(ctx, err) =>
        let d = compose(ctx, d);
        DynamicErrorHole(d, err) |> wrap;
      | StaticErrorHole(i, ctx) =>
        let d = compose(ctx, d);
        StaticErrorHole(i, d) |> wrap;
      | MatchScrut(ctx, rules) =>
        let d = compose(ctx, d);
        Match(d, rules) |> wrap;
      | MatchRule(scr, p, ctx, (lr, rr)) =>
        let d = compose(ctx, d);
        Match(scr, ListUtil.rev_concat(lr, [(p, d), ...rr])) |> wrap;
      }
    );
  };
};
