open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type term =
  | Closure([@show.opaque] ClosureEnvironment.t, t)
  | Filter(TermBase.StepperFilterKind.t(IdTag.t), t)
  | Seq1(t, DHExp.t(IdTag.t))
  | Seq2(DHExp.t(IdTag.t), t)
  | Let1(Pat.t(IdTag.t), t, DHExp.t(IdTag.t))
  | Let2(Pat.t(IdTag.t), DHExp.t(IdTag.t), t)
  | Fun(Pat.t(IdTag.t), t, option(ClosureEnvironment.t), option(Var.t))
  | FixF(Pat.t(IdTag.t), t, option(ClosureEnvironment.t))
  | TypAp(t, Typ.t(IdTag.t))
  | Ap1(Operators.ap_direction, t, DHExp.t(IdTag.t))
  | Ap2(Operators.ap_direction, DHExp.t(IdTag.t), t)
  | DeferredAp1(t, list(DHExp.t(IdTag.t)))
  | DeferredAp2(
      DHExp.t(IdTag.t),
      t,
      (list(DHExp.t(IdTag.t)), list(DHExp.t(IdTag.t))),
    )
  | If1(t, DHExp.t(IdTag.t), DHExp.t(IdTag.t))
  | If2(DHExp.t(IdTag.t), t, DHExp.t(IdTag.t))
  | If3(DHExp.t(IdTag.t), DHExp.t(IdTag.t), t)
  | UnOp(Operators.op_un, t)
  | BinOp1(Operators.op_bin, t, DHExp.t(IdTag.t))
  | BinOp2(Operators.op_bin, DHExp.t(IdTag.t), t)
  | Tuple(t, (list(DHExp.t(IdTag.t)), list(DHExp.t(IdTag.t))))
  | Test(t)
  | ListLit(t, (list(DHExp.t(IdTag.t)), list(DHExp.t(IdTag.t))))
  | MultiHole(t, (list(Any.t(IdTag.t)), list(Any.t(IdTag.t))))
  | Cons1(t, DHExp.t(IdTag.t))
  | Cons2(DHExp.t(IdTag.t), t)
  | ListConcat1(t, DHExp.t(IdTag.t))
  | ListConcat2(DHExp.t(IdTag.t), t)
  | Cast(t, Typ.t(IdTag.t), Typ.t(IdTag.t))
  | FailedCast(t, Typ.t(IdTag.t), Typ.t(IdTag.t))
  | DynamicErrorHole(t, InvalidOperationError.t)
  | MatchScrut(t, list((UPat.t(IdTag.t), DHExp.t(IdTag.t))))
  | MatchRule(
      DHExp.t(IdTag.t),
      UPat.t(IdTag.t),
      t,
      (
        list((UPat.t(IdTag.t), DHExp.t(IdTag.t))),
        list((UPat.t(IdTag.t), DHExp.t(IdTag.t))),
      ),
    )
and t =
  | Mark
  | Term({
      term,
      ids: list(Id.t),
    });

let rec compose = (ctx: t, d: DHExp.t(IdTag.t)): DHExp.t(IdTag.t) => {
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
      | DeferredAp1(ctx, d2s) =>
        let d1 = compose(ctx, d);
        DeferredAp(d1, d2s) |> wrap;
      | DeferredAp2(d1, ctx, (ld, rd)) =>
        let d2 = compose(ctx, d);
        DeferredAp(d1, ListUtil.rev_concat(ld, [d2, ...rd])) |> wrap;
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
      | ListLit(ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        ListLit(ListUtil.rev_concat(ld, [d, ...rd])) |> wrap;
      | MultiHole(ctx, (ld, rd)) =>
        let d = compose(ctx, d);
        MultiHole(ListUtil.rev_concat(ld, [TermBase.Any.Exp(d), ...rd]))
        |> wrap;
      | Let1(dp, ctx, d2) =>
        let d = compose(ctx, d);
        Let(dp, d, d2) |> wrap;
      | Let2(dp, d1, ctx) =>
        let d = compose(ctx, d);
        Let(dp, d1, d) |> wrap;
      | Fun(dp, ctx, env, v) =>
        let d = compose(ctx, d);
        Fun(dp, d, env, v) |> wrap;
      | FixF(v, ctx, env) =>
        let d = compose(ctx, d);
        FixF(v, d, env) |> wrap;
      | Cast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        Cast(d, ty1, ty2) |> wrap;
      | FailedCast(ctx, ty1, ty2) =>
        let d = compose(ctx, d);
        FailedCast(d, ty1, ty2) |> wrap;
      | DynamicErrorHole(ctx, err) =>
        let d = compose(ctx, d);
        DynamicErrorHole(d, err) |> wrap;
      | MatchScrut(ctx, rules) =>
        let d = compose(ctx, d);
        Match(d, rules) |> wrap;
      | MatchRule(scr, p, ctx, (lr, rr)) =>
        let d = compose(ctx, d);
        Match(scr, ListUtil.rev_concat(lr, [(p, d), ...rr])) |> wrap;
      | TypAp(ctx, ty) =>
        let d = compose(ctx, d);
        TypAp(d, ty) |> wrap;
      }
    );
  };
};
