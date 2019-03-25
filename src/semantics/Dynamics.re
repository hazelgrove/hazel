open SemanticsCommon;
open HazelUtil;

type hole_sort =
  | ExpressionHole
  | PatternHole;

module Delta = {
  type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));
  let empty: t = (MetaVarMap.empty: t);
};

/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */
type inst_num = int;

module DHPat = {
  type t =
    | EmptyHole(MetaVar.t, inst_num)
    | NonEmptyHole(in_hole_reason, MetaVar.t, inst_num, t)
    | Wild
    | Keyword(MetaVar.t, inst_num, keyword)
    | Var(Var.t)
    | NumLit(int)
    | BoolLit(bool)
    | Inj(inj_side, t)
    | ListNil
    | Cons(t, t)
    | Pair(t, t)
    | Triv /* unit intro */
    | Ap(t, t);

  let rec make_tuple = (ds: ListMinTwo.t(t)): t =>
    switch (ds) {
    | Pair(d1, d2) => Pair(d1, d2)
    | Cons(d1, ds) =>
      let d2 = make_tuple(ds);
      Pair(d1, d2);
    };

  /* whether dp contains the variable x outside of a hole */
  let rec binds_var = (x: Var.t, dp: t): bool =>
    switch (dp) {
    | EmptyHole(_, _)
    | NonEmptyHole(_, _, _, _)
    | Wild
    | NumLit(_)
    | BoolLit(_)
    | Triv
    | ListNil
    | Keyword(_, _, _) => false
    | Var(y) => Var.eq(x, y)
    | Inj(_, dp1) => binds_var(x, dp1)
    | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
    | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
    | Ap(_, _) => false
    };

  type expand_result =
    | Expands(t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotExpand;

  let rec syn_expand =
          (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): expand_result =>
    switch (p) {
    | Pat(InHole(TypeInconsistent as reason, u), p')
    | Pat(
        InHole(WrongLength as reason, u),
        OpSeq(BinOp(InHole(WrongLength, _), Comma, _, _), _) as p',
      ) =>
      switch (syn_expand'(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      }
    | Pat(InHole(WrongLength, _), _) => DoesNotExpand
    | Pat(NotInHole, p') => syn_expand'(ctx, delta, p')
    | Parenthesized(p1) => syn_expand(ctx, delta, p1)
    }
  and syn_expand' =
      (ctx: Contexts.t, delta: Delta.t, p': UHPat.t'): expand_result =>
    switch (p') {
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | Wild => Expands(Wild, Hole, ctx, delta)
    | Var(InVHole(Free, _), _) => raise(FreeVarInPat)
    | Var(InVHole(Keyword(k), u), _) =>
      Expands(Keyword(u, 0, k), Hole, ctx, delta)
    | Var(NotInVHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, Hole));
      Expands(Var(x), Hole, ctx, delta);
    | NumLit(n) => Expands(NumLit(n), Num, ctx, delta)
    | BoolLit(b) => Expands(BoolLit(b), Bool, ctx, delta)
    | Inj(side, p) =>
      switch (syn_expand(ctx, delta, p)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let dp = Inj(side, dp1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Expands(dp, ty, ctx, delta);
      }
    | ListNil => Expands(ListNil, List(Hole), ctx, delta)
    | OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    }
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel_t, seq: UHPat.opseq)
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(pn) => syn_expand(ctx, delta, pn)
      }
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, ty2, ctx, delta) =>
          let dp = Pair(dp1, dp2);
          Expands(dp, Prod(ty1, ty2), ctx, delta);
        }
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _ty1, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, _ty2, ctx, delta) =>
          let dp = Ap(dp1, dp2);
          Expands(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_expand_skel(ctx, delta, skel2, seq, ty)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, _, ctx, delta) =>
          let dp = Cons(dp1, dp2);
          Expands(dp, ty, ctx, delta);
        };
      }
    }
  and ana_expand =
      (ctx: Contexts.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t): expand_result =>
    switch (p) {
    | Pat(NotInHole, p') => ana_expand'(ctx, delta, p', ty)
    | Pat(InHole(TypeInconsistent as reason, u), p') =>
      switch (syn_expand'(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      }
    | Pat(
        InHole(WrongLength as reason, u),
        OpSeq(BinOp(InHole(WrongLength, _), Comma, _, _), _) as p',
      ) =>
      switch (ana_expand'(ctx, delta, p', ty)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      }
    | Pat(InHole(WrongLength, _), _) => DoesNotExpand
    | Parenthesized(p) => ana_expand(ctx, delta, p, ty)
    }
  and ana_expand' =
      (ctx: Contexts.t, delta: Delta.t, p': UHPat.t', ty: HTyp.t)
      : expand_result =>
    switch (p') {
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | Var(InVHole(Free, _), _) => raise(FreeVarInPat)
    | Var(InVHole(Keyword(k), u), _) =>
      Expands(Keyword(u, 0, k), ty, ctx, delta)
    | Var(NotInVHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      Expands(Var(x), ty, ctx, delta);
    | Wild => Expands(Wild, ty, ctx, delta)
    | NumLit(_)
    | BoolLit(_) => syn_expand'(ctx, delta, p')
    | Inj(side, p1) =>
      switch (HTyp.matched_sum(ty)) {
      | None => DoesNotExpand
      | Some((tyL, tyR)) =>
        let ty1 = pick_side(side, tyL, tyR);
        switch (ana_expand(ctx, delta, p1, ty1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) =>
          let ty =
            switch (side) {
            | L => HTyp.Sum(ty1, tyR)
            | R => HTyp.Sum(tyL, ty1)
            };
          Expands(Inj(side, dp1), ty, ctx, delta);
        };
      }
    | ListNil =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(ty_elt) => Expands(ListNil, HTyp.List(ty_elt), ctx, delta)
      }
    | OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
    }
  and ana_expand_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHPat.skel_t,
        seq: UHPat.opseq,
        ty: HTyp.t,
      )
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(pn) => ana_expand(ctx, delta, pn, ty)
      }
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      };
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (HTyp.matched_prod(ty)) {
      | None => DoesNotExpand
      | Some((ty1, ty2)) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) =>
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp2, ty2, ctx, delta) =>
            let dp = Pair(dp1, dp2);
            Expands(dp, Prod(ty1, ty2), ctx, delta);
          }
        }
      }
    | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
      switch (ty) {
      | HTyp.Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHPat.get_tuple(skel1, skel2);
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let processed1 =
          ListMinTwo.fold_right(
            (skel_ty: (UHPat.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((elts, ctx, delta)) =>
                let (skel, ty) = skel_ty;
                switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
                | DoesNotExpand => None
                | Expands(dp, ty, ctx, delta) =>
                  Some((ListMinTwo.Cons((dp, ty), elts), ctx, delta))
                };
              },
            zipped,
            ((skel1, ty1), (skel2, ty2)) =>
              switch (ana_expand_skel(ctx, delta, skel1, seq, ty1)) {
              | DoesNotExpand => None
              | Expands(dp1, ty1, ctx, delta) =>
                switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
                | DoesNotExpand => None
                | Expands(dp2, ty2, ctx, delta) =>
                  Some((
                    ListMinTwo.Pair((dp1, ty1), (dp2, ty2)),
                    ctx,
                    delta,
                  ))
                }
              },
          );
        switch (processed1) {
        | None => DoesNotExpand
        | Some((elts1, ctx, delta)) =>
          let processed2 =
            List.fold_right(
              (skel: UHPat.skel_t, opt_result) =>
                switch (opt_result) {
                | None => None
                | Some((elts, ctx, delta)) =>
                  switch (syn_expand_skel(ctx, delta, skel, seq)) {
                  | DoesNotExpand => None
                  | Expands(dp, ty, ctx, delta) =>
                    Some(([(dp, ty), ...elts], ctx, delta))
                  }
                },
              remainder,
              Some(([], ctx, delta)),
            );
          switch (processed2) {
          | None => DoesNotExpand
          | Some((elts2, ctx, delta)) =>
            let elts = ListMinTwo.append_list(elts1, elts2);
            let (ds, tys) = ListMinTwo.unzip(elts);
            let d = make_tuple(ds);
            let ty = HTyp.make_tuple(tys);
            Expands(d, ty, ctx, delta);
          };
        };
      | _ => DoesNotExpand
      }
    | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Hole)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _ty1, ctx, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Hole)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, _ty2, ctx, delta) =>
          let dp = Ap(dp1, dp2);
          Expands(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(ty_elt) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty_elt)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp1, _, ctx, delta) =>
          let ty_list = HTyp.List(ty_elt);
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty_list)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp2, _, ctx, delta) =>
            let dp = Cons(dp1, dp2);
            Expands(dp, ty, ctx, delta);
          };
        }
      }
    };
};

module DHExp = {
  type bin_num_op =
    | Plus
    | Times
    | LessThan;

  let of_op = (op: UHExp.op): option((bin_num_op, HTyp.t)) =>
    switch (op) {
    | Plus => Some((Plus, Num))
    | Times => Some((Times, Num))
    | LessThan => Some((LessThan, Bool))
    | Space
    | Cons
    | Comma => None
    };

  let to_op = (bno: bin_num_op): UHExp.op =>
    switch (bno) {
    | Plus => Plus
    | Times => Times
    | LessThan => LessThan
    };

  module DHExp = {
    type t =
      | EmptyHole(MetaVar.t, inst_num, VarMap.t_(t))
      | NonEmptyHole(in_hole_reason, MetaVar.t, inst_num, VarMap.t_(t), t)
      | Keyword(MetaVar.t, inst_num, VarMap.t_(t), keyword)
      | FreeVar(MetaVar.t, inst_num, VarMap.t_(t), Var.t)
      | BoundVar(Var.t)
      | Let(DHPat.t, t, t)
      | FixF(Var.t, HTyp.t, t)
      | Lam(DHPat.t, HTyp.t, t)
      | Ap(t, t)
      | BoolLit(bool)
      | NumLit(int)
      | BinNumOp(bin_num_op, t, t)
      | ListNil(HTyp.t)
      | Cons(t, t)
      | Inj(HTyp.t, inj_side, t)
      | Pair(t, t)
      | Triv
      | Case(t, list(rule), int)
      | Cast(t, HTyp.t, HTyp.t)
      | FailedCast(t, HTyp.t, HTyp.t)
    and rule =
      | Rule(DHPat.t, t);
  };
  include DHExp;

  let constructor_string = (d: t): string =>
    switch (d) {
    | EmptyHole(_, _, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
    | Keyword(_, _, _, _) => "Keyword"
    | FreeVar(_, _, _, _) => "FreeVar"
    | BoundVar(_) => "BoundVar"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Lam(_, _, _) => "Lam"
    | Ap(_, _) => "Ap"
    | BoolLit(_) => "BoolLit"
    | NumLit(_) => "NumLit"
    | BinNumOp(_, _, _) => "BinNumOp"
    | ListNil(_) => "ListNil"
    | Cons(_, _) => "Cons"
    | Inj(_, _, _) => "Inj"
    | Pair(_, _) => "Pair"
    | Triv => "Triv"
    | Case(_, _, _) => "Case"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    };

  let rec make_tuple = (ds: ListMinTwo.t(t)): t =>
    switch (ds) {
    | Pair(d1, d2) => Pair(d1, d2)
    | Cons(d1, ds) =>
      let d2 = make_tuple(ds);
      Pair(d1, d2);
    };

  let cast = (d: t, t1: HTyp.t, t2: HTyp.t): t =>
    if (HTyp.eq(t1, t2)) {
      d;
    } else {
      Cast(d, t1, t2);
    };

  let apply_casts = (d: t, casts: list((HTyp.t, HTyp.t))): t =>
    List.fold_left(
      (d, c: (HTyp.t, HTyp.t)) => {
        let (ty1, ty2) = c;
        cast(d, ty1, ty2);
      },
      d,
      casts,
    );

  module Environment = {
    type t = VarMap.t_(DHExp.t);
    include VarMap;
  };

  /* closed substitution [d1/x]d2*/
  let rec subst_var = (d1: t, x: Var.t, d2: t): t =>
    switch (d2) {
    | BoundVar(y) =>
      if (Var.eq(x, y)) {
        d1;
      } else {
        d2;
      }
    | FreeVar(_, _, _, _) => d2
    | Keyword(_, _, _, _) => d2
    | Let(dp, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 =
        if (DHPat.binds_var(x, dp)) {
          d4;
        } else {
          subst_var(d1, x, d4);
        };
      Let(dp, d3, d4);
    | FixF(y, ty, d3) =>
      let d3 =
        if (Var.eq(x, y)) {
          d3;
        } else {
          subst_var(d1, x, d3);
        };
      FixF(y, ty, d3);
    | Lam(dp, ty, d3) =>
      if (DHPat.binds_var(x, dp)) {
        d2;
      } else {
        let d3 = subst_var(d1, x, d3);
        Lam(dp, ty, d3);
      }
    | Ap(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Ap(d3, d4);
    | BoolLit(_)
    | NumLit(_)
    | ListNil(_)
    | Triv => d2
    | Cons(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Cons(d3, d4);
    | BinNumOp(op, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      BinNumOp(op, d3, d4);
    | Inj(ty, side, d3) =>
      let d3 = subst_var(d1, x, d3);
      Inj(ty, side, d3);
    | Pair(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Pair(d3, d4);
    | Case(d3, rules, n) =>
      let d3 = subst_var(d1, x, d3);
      let rules = subst_var_rules(d1, x, rules);
      Case(d3, rules, n);
    | EmptyHole(u, i, sigma) =>
      let sigma' = subst_var_env(d1, x, sigma);
      EmptyHole(u, i, sigma');
    | NonEmptyHole(reason, u, i, sigma, d3) =>
      let d3' = subst_var(d1, x, d3);
      let sigma' = subst_var_env(d1, x, sigma);
      NonEmptyHole(reason, u, i, sigma', d3');
    | Cast(d, ty1, ty2) =>
      let d' = subst_var(d1, x, d);
      Cast(d', ty1, ty2);
    | FailedCast(d, ty1, ty2) =>
      let d' = subst_var(d1, x, d);
      FailedCast(d', ty1, ty2);
    }
  and subst_var_rules = (d1: t, x: Var.t, rules: list(rule)): list(rule) =>
    List.map(
      (r: rule) =>
        switch (r) {
        | Rule(dp, d2) =>
          if (DHPat.binds_var(x, dp)) {
            r;
          } else {
            Rule(dp, subst_var(d1, x, d2));
          }
        },
      rules,
    )
  and subst_var_env = (d1: t, x: Var.t, sigma: Environment.t): Environment.t =>
    List.map(
      xd => {
        let (y, d) = xd;
        (y, subst_var(d1, x, d));
      },
      sigma,
    );

  let subst = (env: Environment.t, d: t): t =>
    List.fold_left(
      (d2, xd: (Var.t, t)) => {
        let (x, d1) = xd;
        subst_var(d1, x, d2);
      },
      d,
      env,
    );

  type match_result =
    | Matches(Environment.t)
    | DoesNotMatch
    | Indet;

  let rec matches = (dp: DHPat.t, d: t): match_result =>
    switch (dp, d) {
    | (_, BoundVar(_)) => DoesNotMatch
    | (EmptyHole(_, _), _)
    | (NonEmptyHole(_, _, _, _), _) => Indet
    | (Wild, _) => Matches(Environment.empty)
    | (Keyword(_, _, _), _) => DoesNotMatch
    | (Var(x), _) =>
      let env = Environment.extend(Environment.empty, (x, d));
      Matches(env);
    | (_, EmptyHole(_, _, _)) => Indet
    | (_, NonEmptyHole(_, _, _, _, _)) => Indet
    | (_, FailedCast(_, _, _)) => Indet
    | (_, FreeVar(_, _, _, _)) => Indet
    | (_, Let(_, _, _)) => Indet
    | (_, FixF(_, _, _)) => DoesNotMatch
    | (_, Lam(_, _, _)) => DoesNotMatch
    | (_, Ap(_, _)) => Indet
    | (_, BinNumOp(_, _, _)) => Indet
    | (_, Case(_, _, _)) => Indet
    | (BoolLit(b1), BoolLit(b2)) =>
      if (b1 == b2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (BoolLit(_), Cast(d, Bool, Hole)) => matches(dp, d)
    | (BoolLit(_), Cast(d, Hole, Bool)) => matches(dp, d)
    | (BoolLit(_), _) => DoesNotMatch
    | (NumLit(n1), NumLit(n2)) =>
      if (n1 == n2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (NumLit(_), Cast(d, Num, Hole)) => matches(dp, d)
    | (NumLit(_), Cast(d, Hole, Num)) => matches(dp, d)
    | (NumLit(_), _) => DoesNotMatch
    | (Inj(side1, dp), Inj(_, side2, d)) =>
      switch (side1, side2) {
      | (L, L)
      | (R, R) => matches(dp, d)
      | _ => DoesNotMatch
      }
    | (Inj(side, dp), Cast(d, Sum(tyL1, tyR1), Sum(tyL2, tyR2))) =>
      matches_cast_Inj(side, dp, d, [(tyL1, tyR1, tyL2, tyR2)])
    | (Inj(_, _), Cast(d, Sum(_, _), Hole)) => matches(dp, d)
    | (Inj(_, _), Cast(d, Hole, Sum(_, _))) => matches(dp, d)
    | (Inj(_, _), _) => DoesNotMatch
    | (Pair(dp1, dp2), Pair(d1, d2)) =>
      switch (matches(dp1, d1)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env1) =>
        switch (matches(dp2, d2)) {
        | DoesNotMatch => DoesNotMatch
        | Indet => Indet
        | Matches(env2) => Matches(Environment.union(env1, env2))
        }
      }
    | (Pair(dp1, dp2), Cast(d, Prod(tyL1, tyR1), Prod(tyL2, tyR2))) =>
      matches_cast_Pair(dp1, dp2, d, [(tyL1, tyL2)], [(tyR1, tyR2)])
    | (Pair(_, _), Cast(d, Hole, Prod(_, _))) => matches(dp, d)
    | (Pair(_, _), Cast(d, Prod(_, _), Hole)) => matches(dp, d)
    | (Pair(_, _), _) => DoesNotMatch
    | (Triv, Triv) => Matches(Environment.empty)
    | (Triv, Cast(d, Hole, Unit)) => matches(dp, d)
    | (Triv, Cast(d, Unit, Hole)) => matches(dp, d)
    | (Triv, _) => DoesNotMatch
    | (ListNil, ListNil(_)) => Matches(Environment.empty)
    | (ListNil, Cast(d, Hole, List(_))) => matches(dp, d)
    | (ListNil, Cast(d, List(_), Hole)) => matches(dp, d)
    | (ListNil, Cast(d, List(_), List(_))) => matches(dp, d)
    | (ListNil, _) => DoesNotMatch
    | (Cons(dp1, dp2), Cons(d1, d2)) =>
      switch (matches(dp1, d1)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env1) =>
        switch (matches(dp2, d2)) {
        | DoesNotMatch => DoesNotMatch
        | Indet => Indet
        | Matches(env2) => Matches(Environment.union(env1, env2))
        }
      }
    | (Cons(dp1, dp2), Cast(d, List(ty1), List(ty2))) =>
      matches_cast_Cons(dp1, dp2, d, [(ty1, ty2)])
    | (Cons(_, _), Cast(d, Hole, List(_))) => matches(dp, d)
    | (Cons(_, _), Cast(d, List(_), Hole)) => matches(dp, d)
    | (Cons(_, _), _) => DoesNotMatch
    | (Ap(_, _), _) => DoesNotMatch
    }
  and matches_cast_Inj =
      (
        side: inj_side,
        dp: DHPat.t,
        d: t,
        casts: list((HTyp.t, HTyp.t, HTyp.t, HTyp.t)),
      )
      : match_result =>
    switch (d) {
    | Inj(_, side', d') =>
      switch (side, side') {
      | (L, L)
      | (R, R) =>
        let side_casts =
          List.map(
            (c: (HTyp.t, HTyp.t, HTyp.t, HTyp.t)) => {
              let (tyL1, tyR1, tyL2, tyR2) = c;
              switch (side) {
              | L => (tyL1, tyL2)
              | R => (tyR1, tyR2)
              };
            },
            casts,
          );
        matches(dp, apply_casts(d', side_casts));
      | _ => DoesNotMatch
      }
    | Cast(d', Sum(tyL1, tyR1), Sum(tyL2, tyR2)) =>
      matches_cast_Inj(side, dp, d', [(tyL1, tyR1, tyL2, tyR2), ...casts])
    | Cast(d', Sum(_, _), Hole)
    | Cast(d', Hole, Sum(_, _)) => matches_cast_Inj(side, dp, d', casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinNumOp(_, _, _) => Indet
    | BoolLit(_) => DoesNotMatch
    | NumLit(_) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Cons(_, _) => DoesNotMatch
    | Pair(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | Case(_, _, _) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    }
  and matches_cast_Pair =
      (
        dp1: DHPat.t,
        dp2: DHPat.t,
        d: t,
        left_casts: list((HTyp.t, HTyp.t)),
        right_casts: list((HTyp.t, HTyp.t)),
      )
      : match_result =>
    switch (d) {
    | Pair(d1, d2) =>
      switch (matches(dp1, apply_casts(d1, left_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env1) =>
        switch (matches(dp2, apply_casts(d2, right_casts))) {
        | DoesNotMatch => DoesNotMatch
        | Indet => Indet
        | Matches(env2) => Matches(Environment.union(env1, env2))
        }
      }
    | Cast(d', Prod(tyL1, tyR1), Prod(tyL2, tyR2)) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d',
        [(tyL1, tyL2), ...left_casts],
        [(tyR1, tyR2), ...right_casts],
      )
    | Cast(d', Prod(_, _), Hole)
    | Cast(d', Hole, Prod(_, _)) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinNumOp(_, _, _) => Indet
    | BoolLit(_) => DoesNotMatch
    | NumLit(_) => DoesNotMatch
    | Inj(_, _, _) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Cons(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | Case(_, _, _) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    }
  and matches_cast_Cons =
      (dp1: DHPat.t, dp2: DHPat.t, d: t, elt_casts: list((HTyp.t, HTyp.t)))
      : match_result =>
    switch (d) {
    | Cons(d1, d2) =>
      switch (matches(dp1, apply_casts(d1, elt_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env1) =>
        let list_casts =
          List.map(
            (c: (HTyp.t, HTyp.t)) => {
              let (ty1, ty2) = c;
              (HTyp.List(ty1), HTyp.List(ty2));
            },
            elt_casts,
          );
        switch (matches(dp2, apply_casts(d2, list_casts))) {
        | DoesNotMatch => DoesNotMatch
        | Indet => Indet
        | Matches(env2) => Matches(Environment.union(env1, env2))
        };
      }
    | Cast(d', List(ty1), List(ty2)) =>
      matches_cast_Cons(dp1, dp2, d', [(ty1, ty2), ...elt_casts])
    | Cast(d', List(_), Hole) => matches_cast_Cons(dp1, dp2, d', elt_casts)
    | Cast(d', Hole, List(_)) => matches_cast_Cons(dp1, dp2, d', elt_casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinNumOp(_, _, _) => Indet
    | BoolLit(_) => DoesNotMatch
    | NumLit(_) => DoesNotMatch
    | Inj(_, _, _) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Pair(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | Case(_, _, _) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    };

  type expand_result =
    | Expands(t, HTyp.t, Delta.t)
    | DoesNotExpand;

  let id_env = (ctx: VarCtx.t): Environment.t =>
    VarMap.map(
      xt => {
        let (x, _) = xt;
        DHExp.BoundVar(x);
      },
      ctx,
    );

  let rec syn_expand =
          (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): expand_result =>
    switch (e) {
    | Parenthesized(e1) => syn_expand(ctx, delta, e1)
    | Tm(NotInHole, e') => syn_expand'(ctx, delta, e')
    | Tm(InHole(TypeInconsistent as reason, u), e')
    | Tm(
        InHole(WrongLength as reason, u),
        OpSeq(BinOp(InHole(WrongLength, _), Comma, _, _), _) as e',
      ) =>
      switch (syn_expand'(ctx, delta, e')) {
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      | DoesNotExpand => DoesNotExpand
      }
    | Tm(InHole(WrongLength, _), _) => DoesNotExpand
    }
  and syn_expand' =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t'): expand_result =>
    switch (e) {
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = DHExp.EmptyHole(u, 0, sigma);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | Var(NotInVHole, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x)) {
      | Some(ty) => Expands(BoundVar(x), ty, delta)
      | None => DoesNotExpand
      };
    | Var(InVHole(reason, u), x) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, Hole, gamma)));
      let d =
        switch (reason) {
        | Free => DHExp.FreeVar(u, 0, sigma, x)
        | Keyword(k) => DHExp.Keyword(u, 0, sigma, k)
        };
      Expands(d, Hole, delta);
    | Lam(p, ann, e1) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        switch (syn_expand(ctx, delta, e1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty2, delta) =>
          let d = Lam(dp, ty1, d1);
          Expands(d, Arrow(ty1, ty2), delta);
        }
      };
    | LineItem(EmptyLine, e1) => syn_expand(ctx, delta, e1)
    | LineItem(ExpLine(e1), e2) =>
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, _, delta) =>
        switch (syn_expand(ctx, delta, e2)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let d = Let(Wild, d1, d2);
          Expands(d, ty2, delta);
        }
      }
    | LineItem(LetLine(p, ann, e1), e2) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let (ctx1, is_recursive_fn) = Statics.ctx_for_let'(ctx, p, ty1, e1);
        switch (ana_expand(ctx1, delta, e1, ty1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1', delta) =>
          let d1 =
            switch (is_recursive_fn) {
            | None => d1
            | Some(x) =>
              FixF(x, ty1', subst_var(cast(BoundVar(x), ty1', ty1), x, d1))
            };
          let d1 = cast(d1, ty1', ty1);
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp, _, ctx, delta) =>
            switch (syn_expand(ctx, delta, e2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, delta) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          };
        };
      | None =>
        switch (syn_expand(ctx, delta, e1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1, delta) =>
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp, _, ctx, delta) =>
            switch (syn_expand(ctx, delta, e2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, _delta2) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | NumLit(n) => Expands(NumLit(n), Num, delta)
    | BoolLit(b) => Expands(BoolLit(b), Bool, delta)
    | Inj(side, e1) =>
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let d = DHExp.Inj(Hole, side, d1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Expands(d, ty, delta);
      }
    | ListNil =>
      let elt_ty = HTyp.Hole;
      Expands(ListNil(elt_ty), List(elt_ty), delta);
    | Case(e1, rules, Some(uty)) =>
      let ty = UHTyp.expand(uty);
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(ctx, delta, rules, ty1, ty)) {
        | None => DoesNotExpand
        | Some((drs, delta)) =>
          let d = Case(d1, drs, 0);
          Expands(d, ty, delta);
        }
      };
    | Case(_, _, None) => DoesNotExpand
    | OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    | ApPalette(_name, _serialized_model, _hole_data) => DoesNotExpand
    /* TODO fix me */
    /* let (_, palette_ctx) = ctx in
       begin match (VarMap.lookup palette_ctx name) with
       | Some palette_defn ->
         let expansion_ty = UHExp.PaletteDefinition.expansion_ty palette_defn in
         let to_exp = UHExp.PaletteDefinition.to_exp palette_defn in
         let expansion = to_exp serialized_model in
         let (_, hole_map) = hole_data in
         (* bind each free variable in expansion by wrapping expansion
          * in lambda, then apply lambda to args in hole data
          *)
         let bound_expansion :=
             NatMap.fold hole_map
               (fun bound entry ->
                 let (n, typ_exp) = entry in
                 let (htyp, hexp) = typ_exp in
                 let lam = UHExp.Tm NotInHole (UHExp.Lam (UHExp.PaletteHoleData.mk_hole_ref_var_name n) bound) in
                 let hexp_ann = UHExp.Tm NotInHole (UHExp.Asc (UHExp.Parenthesized hexp) (UHTyp.contract htyp)) in
                 let opseq = OperatorSeq.ExpOpExp (UHExp.Parenthesized lam) UHExp.Space (UHExp.Parenthesized hexp_ann) in
                 let ap = UHExp.OpSeq (Associator.associate_exp opseq) opseq in
                 UHExp.Tm NotInHole ap
               )
               expansion in
         ana_expand ctx bound_expansion expansion_ty
       | None -> DoesNotExpand
       end */
    }
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHExp.skel_t, seq: UHExp.opseq)
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => syn_expand(ctx, delta, en)
      }
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (Statics.syn_skel(ctx, skel1, seq, None)) {
      | None => DoesNotExpand
      | Some((ty1, _)) =>
        switch (HTyp.matched_arrow(ty1)) {
        | None => DoesNotExpand
        | Some((ty2, ty)) =>
          let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
          switch (ana_expand_skel(ctx, delta, skel1, seq, ty2_arrow_ty)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(d1, ty1', delta) =>
            switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty2', delta) =>
              let dc1 = cast(d1, ty1', ty2_arrow_ty);
              let dc2 = cast(d2, ty2', ty2);
              let d = Ap(dc1, dc2);
              Expands(d, ty, delta);
            }
          };
        }
      }
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let d = Pair(d1, d2);
          let ty = HTyp.Prod(ty1, ty2);
          Expands(d, ty, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_expand_skel(ctx, delta, skel2, seq, ty)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let d2c = cast(d2, ty2, ty);
          let d = Cons(d1, d2c);
          Expands(d, ty, delta);
        };
      }
    | BinOp(NotInHole, Plus as op, skel1, skel2)
    | BinOp(NotInHole, Times as op, skel1, skel2)
    | BinOp(NotInHole, LessThan as op, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Num)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Num)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = cast(d1, ty1, Num);
          let dc2 = cast(d2, ty2, Num);
          switch (of_op(op)) {
          | None => DoesNotExpand
          | Some((op, ty)) =>
            let d = BinNumOp(op, dc1, dc2);
            Expands(d, ty, delta);
          };
        }
      }
    }
  and ana_expand =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t, ty: HTyp.t): expand_result =>
    switch (e) {
    | Tm(NotInHole, e') => ana_expand'(ctx, delta, e', ty)
    | Tm(InHole(TypeInconsistent as reason, u), e') =>
      switch (syn_expand'(ctx, delta, e')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        Expands(NonEmptyHole(reason, u, 0, sigma, d), ty, delta);
      }
    | Tm(
        InHole(WrongLength as reason, u),
        OpSeq(BinOp(InHole(WrongLength, _), Comma, _, _), _) as e',
      ) =>
      switch (ana_expand'(ctx, delta, e', ty)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        let d = NonEmptyHole(reason, u, 0, sigma, d1);
        Expands(d, ty, delta);
      }
    | Tm(InHole(WrongLength, _), _) => DoesNotExpand
    | Parenthesized(e1) => ana_expand(ctx, delta, e1, ty)
    }
  and ana_expand' =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t', ty: HTyp.t)
      : expand_result =>
    switch (e) {
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = EmptyHole(u, 0, sigma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | Var(InVHole(reason, u), x) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      let d =
        switch (reason) {
        | Free => FreeVar(u, 0, sigma, x)
        | Keyword(k) => Keyword(u, 0, sigma, k)
        };
      Expands(d, ty, delta);
    | LineItem(EmptyLine, e1) => ana_expand(ctx, delta, e1, ty)
    | LineItem(ExpLine(e1), e2) =>
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, _, delta) =>
        switch (ana_expand(ctx, delta, e2, ty)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty, delta) =>
          let d = Let(Wild, d1, d2);
          Expands(d, ty, delta);
        }
      }
    | LineItem(LetLine(p, ann, e1), e2) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let (ctx1, is_recursive_fn) = Statics.ctx_for_let'(ctx, p, ty1, e1);
        switch (ana_expand(ctx1, delta, e1, ty1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1', delta) =>
          let d1 =
            switch (is_recursive_fn) {
            | None => d1
            | Some(x) =>
              FixF(x, ty1', subst_var(cast(BoundVar(x), ty1', ty1), x, d1))
            };
          let d1 = cast(d1, ty1', ty1);
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp, _, ctx, delta) =>
            switch (ana_expand(ctx, delta, e2, ty)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, delta) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          };
        };
      | None =>
        switch (syn_expand(ctx, delta, e1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1, delta) =>
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp, _ty1, ctx, delta) =>
            switch (ana_expand(ctx, delta, e2, ty)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, delta) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | Lam(p, ann, e1) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => DoesNotExpand
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          switch (HTyp.consistent(ty1_ann, ty1_given)) {
          | false => DoesNotExpand
          | true =>
            switch (DHPat.ana_expand(ctx, delta, p, ty1_ann)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(dp, ty1p, ctx, delta) =>
              switch (ana_expand(ctx, delta, e1, ty2)) {
              | DoesNotExpand => DoesNotExpand
              | Expands(d1, ty2, delta) =>
                let ty = HTyp.Arrow(ty1p, ty2);
                let d = Lam(dp, ty1p, d1);
                Expands(d, ty, delta);
              }
            }
          };
        | None =>
          switch (DHPat.ana_expand(ctx, delta, p, ty1_given)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(dp, ty1, ctx, delta) =>
            switch (ana_expand(ctx, delta, e1, ty2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d1, ty2, delta) =>
              let ty = HTyp.Arrow(ty1, ty2);
              let d = Lam(dp, ty1, d1);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | Inj(side, e1) =>
      switch (HTyp.matched_sum(ty)) {
      | None => DoesNotExpand
      | Some((ty1, ty2)) =>
        let e1ty = pick_side(side, ty1, ty2);
        switch (ana_expand(ctx, delta, e1, e1ty)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, e1ty', delta) =>
          let (ann_ty, ty) =
            switch (side) {
            | L => (ty2, HTyp.Sum(e1ty', ty2))
            | R => (ty1, HTyp.Sum(ty1, e1ty'))
            };
          let d = Inj(ann_ty, side, d1);
          Expands(d, ty, delta);
        };
      }
    | ListNil =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(elt_ty) => Expands(ListNil(elt_ty), List(elt_ty), delta)
      }
    | Case(e1, rules, Some(uty)) =>
      let ty2 = UHTyp.expand(uty);
      switch (HTyp.consistent(ty, ty2)) {
      | false => DoesNotExpand
      | true =>
        switch (syn_expand(ctx, delta, e1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1, delta) =>
          switch (ana_expand_rules(ctx, delta, rules, ty1, ty2)) {
          | None => DoesNotExpand
          | Some((drs, delta)) =>
            let d = Case(d1, drs, 0);
            Expands(d, ty, delta);
          }
        }
      };
    | Case(e1, rules, None) =>
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(ctx, delta, rules, ty1, ty)) {
        | None => DoesNotExpand
        | Some((drs, delta)) =>
          let d = Case(d1, drs, 0);
          Expands(d, ty, delta);
        }
      }
    | OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
    | Var(NotInVHole, _)
    | BoolLit(_)
    | NumLit(_)
    | ApPalette(_, _, _) =>
      /* subsumption */
      syn_expand'(ctx, delta, e)
    }
  and ana_expand_rules =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        rules: list(UHExp.rule),
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((list(rule), Delta.t)) =>
    List.fold_left(
      (b, r) =>
        switch (b) {
        | None => None
        | Some((drs, delta)) =>
          switch (ana_expand_rule(ctx, delta, r, pat_ty, clause_ty)) {
          | None => None
          | Some((dr, delta)) =>
            let drs = drs @ [dr];
            Some((drs, delta));
          }
        },
      Some(([], delta)),
      rules,
    )
  and ana_expand_rule =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        r: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((rule, Delta.t)) => {
    let UHExp.Rule(p, e) = r;
    switch (DHPat.ana_expand(ctx, delta, p, pat_ty)) {
    | DoesNotExpand => None
    | Expands(dp, _, ctx, delta) =>
      switch (ana_expand(ctx, delta, e, clause_ty)) {
      | DoesNotExpand => None
      | Expands(d1, ty1, delta) =>
        Some((Rule(dp, cast(d1, ty1, clause_ty)), delta))
      }
    };
  }
  and ana_expand_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHExp.skel_t,
        seq: UHExp.opseq,
        ty: HTyp.t,
      )
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => ana_expand(ctx, delta, en, ty)
      }
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1);
        Expands(d, ty, delta);
      };
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (HTyp.matched_prod(ty)) {
      | None => DoesNotExpand
      | Some((ty1, ty2)) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty1, delta) =>
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(d2, ty2, delta) =>
            let d = Pair(d1, d2);
            Expands(d, Prod(ty1, ty2), delta);
          }
        }
      }
    | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
      switch (ty) {
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let processed1 =
          ListMinTwo.fold_right(
            (skel_ty: (UHExp.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((elts, delta)) =>
                let (skel, ty) = skel_ty;
                switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
                | DoesNotExpand => None
                | Expands(d, ty, delta) =>
                  Some((ListMinTwo.Cons((d, ty), elts), delta))
                };
              },
            zipped,
            ((skel1, ty1), (skel2, ty2)) =>
              switch (ana_expand_skel(ctx, delta, skel1, seq, ty1)) {
              | DoesNotExpand => None
              | Expands(d1, ty1, delta) =>
                switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
                | DoesNotExpand => None
                | Expands(d2, ty2, delta) =>
                  Some((ListMinTwo.Pair((d1, ty1), (d2, ty2)), delta))
                }
              },
          );
        switch (processed1) {
        | None => DoesNotExpand
        | Some((elts1, delta)) =>
          let processed2 =
            List.fold_right(
              (skel: UHExp.skel_t, opt_result) =>
                switch (opt_result) {
                | None => None
                | Some((elts, delta)) =>
                  switch (syn_expand_skel(ctx, delta, skel, seq)) {
                  | DoesNotExpand => None
                  | Expands(d, ty, delta) =>
                    Some(([(d, ty), ...elts], delta))
                  }
                },
              remainder,
              Some(([], delta)),
            );
          switch (processed2) {
          | None => DoesNotExpand
          | Some((elts2, delta)) =>
            let elts = ListMinTwo.append_list(elts1, elts2);
            let (ds, tys) = ListMinTwo.unzip(elts);
            let d = make_tuple(ds);
            let ty = HTyp.make_tuple(tys);
            Expands(d, ty, delta);
          };
        };
      | _ => DoesNotExpand
      }
    | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(ty_elt) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty_elt)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty_elt', delta) =>
          let d1c = cast(d1, ty_elt', ty_elt);
          let ty_list = HTyp.List(ty_elt);
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty_list)) {
          | DoesNotExpand => DoesNotExpand
          | Expands(d2, ty2, delta) =>
            let d2c = cast(d2, ty2, ty_list);
            let d = Cons(d1c, d2c);
            Expands(d, ty_list, delta);
          };
        }
      }
    | BinOp(_, Plus, _, _)
    | BinOp(_, Times, _, _)
    | BinOp(_, LessThan, _, _)
    | BinOp(_, Space, _, _) =>
      switch (syn_expand_skel(ctx, delta, skel, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, ty', delta) =>
        if (HTyp.consistent(ty, ty')) {
          Expands(d, ty', delta);
        } else {
          DoesNotExpand;
        }
      }
    };

  module HoleInstance = {
    type t = (MetaVar.t, inst_num);
  };

  module InstancePath = {
    type t = list((HoleInstance.t, Var.t));
  };

  module HoleInstanceInfo = {
    type t = MetaVarMap.t(list((Environment.t, InstancePath.t)));

    let empty: t = (MetaVarMap.empty: t);

    let next =
        (hii: t, u: MetaVar.t, sigma: Environment.t, path: InstancePath.t)
        : (int, t) => {
      let (envs, hii) =
        MetaVarMap.insert_or_map(
          hii,
          u,
          _ => [(sigma, path)],
          envs => [(sigma, path), ...envs],
        );
      (List.length(envs) - 1, hii);
    };

    let update_environment =
        (hii: t, inst: HoleInstance.t, sigma: Environment.t): t => {
      let (u, i) = inst;
      let (_, hii) =
        MetaVarMap.update_with(
          instances => {
            let length = List.length(instances);
            HazelUtil.update_nth(
              length - i - 1,
              instances,
              (inst_info: (Environment.t, InstancePath.t)) => {
                let (_, path) = inst_info;
                (sigma, path);
              },
            );
          },
          u,
          hii,
          [],
        );
      hii;
    };

    let num_instances = (hii: t, u: MetaVar.t): int =>
      switch (MetaVarMap.lookup(hii, u)) {
      | Some(envs) => List.length(envs)
      | None => 0
      };

    let default_instance = (hii: t, u: MetaVar.t): option((MetaVar.t, int)) =>
      switch (MetaVarMap.lookup(hii, u)) {
      | Some(envs) =>
        switch (envs) {
        | [] => None
        | [_, ..._] => Some((u, 0))
        }
      | None => None
      };

    let lookup =
        (hii: t, inst: HoleInstance.t)
        : option((Environment.t, InstancePath.t)) => {
      let (u, i) = inst;
      switch (MetaVarMap.lookup(hii, u)) {
      | Some(envs) =>
        let length = List.length(envs);
        List.nth_opt(envs, length - i - 1);
      | None => None
      };
    };
  };

  let rec renumber_result_only_pat =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, dp: DHPat.t)
          : (DHPat.t, HoleInstanceInfo.t) =>
    switch (dp) {
    | Wild
    | Var(_)
    | NumLit(_)
    | BoolLit(_)
    | ListNil
    | Triv => (dp, hii)
    | EmptyHole(u, _) =>
      /* TODO: Pattern holes don't need environments. Maybe this calls
       * for a refactoring of types to reflect this, e.g., a separate
       * PatHoleInstance type. Passing in an empty environment for now. */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (EmptyHole(u, i), hii);
    | NonEmptyHole(reason, u, _, dp1) =>
      /* TODO: see above */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      (NonEmptyHole(reason, u, i, dp1), hii);
    | Keyword(u, _, k) =>
      /* TODO: see above */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (Keyword(u, i, k), hii);
    | Inj(side, dp1) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      (Inj(side, dp1), hii);
    | Pair(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (Pair(dp1, dp2), hii);
    | Cons(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (Cons(dp1, dp2), hii);
    | Ap(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (Pair(dp1, dp2), hii);
    };

  let rec renumber_result_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
          : (DHExp.t, HoleInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | NumLit(_)
    | ListNil(_)
    | Triv => (d, hii)
    | Let(dp, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (Let(dp, d1, d2), hii);
    | FixF(x, ty, d1) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (FixF(x, ty, d1), hii);
    | Lam(x, ty, d1) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (Lam(x, ty, d1), hii);
    | Ap(d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (Ap(d1, d2), hii);
    | BinNumOp(op, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (BinNumOp(op, d1, d2), hii);
    | Inj(ty, side, d1) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (Inj(ty, side, d1), hii);
    | Pair(d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (Pair(d1, d2), hii);
    | Cons(d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (Cons(d1, d2), hii);
    | Case(d1, rules, n) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (drules, hii) = renumber_result_only_rules(path, hii, rules);
      (Case(d1, drules, n), hii);
    | EmptyHole(u, _, sigma) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (EmptyHole(u, i, sigma), hii);
    | NonEmptyHole(reason, u, _, sigma, d1) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (NonEmptyHole(reason, u, i, sigma, d1), hii);
    | FreeVar(u, _, sigma, x) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (FreeVar(u, i, sigma, x), hii);
    | Keyword(u, _, sigma, k) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (Keyword(u, i, sigma, k), hii);
    | Cast(d1, ty1, ty2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (Cast(d1, ty1, ty2), hii);
    | FailedCast(d1, ty1, ty2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      (FailedCast(d1, ty1, ty2), hii);
    }
  and renumber_result_only_rules =
      (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(rule))
      : (list(rule), HoleInstanceInfo.t) =>
    List.fold_left(
      (b, r) => {
        let (rs, hii) = b;
        switch (r) {
        | Rule(dp, d) =>
          let (dp, hii) = renumber_result_only_pat(path, hii, dp);
          let (d, hii) = renumber_result_only(path, hii, d);
          (rs @ [Rule(dp, d)], hii);
        };
      },
      ([], hii),
      rules,
    );

  let rec renumber_sigmas_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
          : (DHExp.t, HoleInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | NumLit(_)
    | ListNil(_)
    | Triv => (d, hii)
    | Let(dp, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (Let(dp, d1, d2), hii);
    | FixF(x, ty, d1) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (FixF(x, ty, d1), hii);
    | Lam(x, ty, d1) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (Lam(x, ty, d1), hii);
    | Ap(d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (Ap(d1, d2), hii);
    | BinNumOp(op, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (BinNumOp(op, d1, d2), hii);
    | Inj(ty, side, d1) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (Inj(ty, side, d1), hii);
    | Pair(d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (Pair(d1, d2), hii);
    | Cons(d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (Cons(d1, d2), hii);
    | Case(d1, rules, n) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (rules, hii) = renumber_sigmas_only_rules(path, hii, rules);
      (Case(d1, rules, n), hii);
    | EmptyHole(u, i, sigma) =>
      let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (EmptyHole(u, i, sigma), hii);
    | NonEmptyHole(reason, u, i, sigma, d1) =>
      let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (NonEmptyHole(reason, u, i, sigma, d1), hii);
    | FreeVar(u, i, sigma, x) =>
      let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (FreeVar(u, i, sigma, x), hii);
    | Keyword(u, i, sigma, k) =>
      let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (Keyword(u, i, sigma, k), hii);
    | Cast(d1, ty1, ty2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (Cast(d1, ty1, ty2), hii);
    | FailedCast(d1, ty1, ty2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      (FailedCast(d1, ty1, ty2), hii);
    }
  and renumber_sigmas_only_rules =
      (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(rule))
      : (list(rule), HoleInstanceInfo.t) =>
    List.fold_left(
      (b, r) => {
        let (rs, hii) = b;
        switch (r) {
        | Rule(dp, d) =>
          /* pattern holes don't have environments */
          let (d, hii) = renumber_sigmas_only(path, hii, d);
          (rs @ [Rule(dp, d)], hii);
        };
      },
      ([], hii),
      rules,
    )
  and renumber_sigma =
      (
        path: InstancePath.t,
        u: MetaVar.t,
        i: inst_num,
        hii: HoleInstanceInfo.t,
        sigma: Environment.t,
      )
      : (Environment.t, HoleInstanceInfo.t) => {
    let (sigma, hii) =
      List.fold_right(
        (xd: (Var.t, DHExp.t), acc: (Environment.t, HoleInstanceInfo.t)) => {
          let (x, d) = xd;
          let (sigma_in, hii) = acc;
          let path = [((u, i), x), ...path];
          let (d, hii) = renumber_result_only(path, hii, d);
          let sigma_out = [(x, d), ...sigma_in];
          (sigma_out, hii);
        },
        sigma,
        ([], hii),
      );

    List.fold_right(
      (xd: (Var.t, DHExp.t), acc: (Environment.t, HoleInstanceInfo.t)) => {
        let (x, d) = xd;
        let (sigma_in, hii) = acc;
        let path = [((u, i), x), ...path];
        let (d, hii) = renumber_sigmas_only(path, hii, d);
        let sigma_out = [(x, d), ...sigma_in];
        (sigma_out, hii);
      },
      sigma,
      ([], hii),
    );
  };

  let renumber =
      (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
      : (t, HoleInstanceInfo.t) => {
    let (d, hii) = renumber_result_only(path, hii, d);
    renumber_sigmas_only(path, hii, d);
  };
};

module Evaluator = {
  type result =
    | InvalidInput(int)
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t);

  /*
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a number literal 2
     4 = boxed value not a number literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
   */

  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(HTyp.t); /* the argument is the corresponding ground type */

  let grounded_Arrow = NotGroundOrHole(Arrow(Hole, Hole));
  let grounded_Sum = NotGroundOrHole(Sum(Hole, Hole));
  let grounded_Prod = NotGroundOrHole(Prod(Hole, Hole));
  let grounded_List = NotGroundOrHole(List(Hole));

  let ground_cases_of = (ty: HTyp.t): ground_cases =>
    switch (ty) {
    | Hole => Hole
    | Bool
    | Num
    | Unit
    | Arrow(Hole, Hole)
    | Sum(Hole, Hole)
    | Prod(Hole, Hole)
    | List(Hole) => Ground
    | Arrow(_, _) => grounded_Arrow
    | Sum(_, _) => grounded_Sum
    | Prod(_, _) => grounded_Prod
    | List(_) => grounded_List
    };

  let eval_bin_num_op = (op: DHExp.bin_num_op, n1: int, n2: int): DHExp.t =>
    switch (op) {
    | Plus => NumLit(n1 + n2)
    | Times => NumLit(n1 * n2)
    | LessThan => BoolLit(n1 < n2)
    };

  let rec evaluate = (d: DHExp.t): result =>
    switch (d) {
    | BoundVar(_) => InvalidInput(1)
    | Let(dp, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (DHExp.matches(dp, d1)) {
        | Indet => Indet(d)
        | DoesNotMatch => Indet(d)
        | Matches(env) => evaluate(DHExp.subst(env, d2))
        }
      }
    | FixF(x, _ty, d1) => evaluate(DHExp.subst_var(d, x, d1))
    | Lam(_, _, _) => BoxedValue(d)
    | Ap(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(Lam(dp, _tau, d3)) =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2)
        | Indet(d2) =>
          switch (DHExp.matches(dp, d2)) {
          | DoesNotMatch => Indet(d)
          | Indet => Indet(d)
          | Matches(env) =>
            /* beta rule */
            evaluate(DHExp.subst(env, d3))
          }
        }
      | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
      | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') =>
          /* ap cast rule */
          evaluate(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
        }
      | BoxedValue(_) => InvalidInput(2)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(Ap(d1', d2'))
        }
      }
    | ListNil(_)
    | BoolLit(_)
    | NumLit(_)
    | Triv => BoxedValue(d)
    | BinNumOp(op, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(NumLit(n1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(NumLit(n2)) => BoxedValue(eval_bin_num_op(op, n1, n2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(BinNumOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinNumOp(op, d1', d2'))
        }
      }
    | Inj(ty, side, d1) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1'))
      | Indet(d1') => Indet(Inj(ty, side, d1'))
      }
    | Pair(d1, d2) =>
      switch (evaluate(d1), evaluate(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
      }
    | Cons(d1, d2) =>
      switch (evaluate(d1), evaluate(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Cons(d1, d2))
      }
    | Case(d1, rules, n) => evaluate_case(d1, rules, n)
    | EmptyHole(_, _, _) => Indet(d)
    | NonEmptyHole(reason, u, i, sigma, d1) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
      }
    | FreeVar(_, _, _, _) => Indet(d)
    | Keyword(_, _, _, _) => Indet(d)
    | Cast(d1, ty, ty') =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1') as result =>
        switch (ground_cases_of(ty), ground_cases_of(ty')) {
        | (Hole, Hole) => result
        | (Ground, Ground) =>
          /* if two types are ground and consistent, then they are eq */
          result
        | (Ground, Hole) =>
          /* can't remove the cast or do anything else here, so we're done */
          BoxedValue(Cast(d1', ty, ty'))
        | (Hole, Ground) =>
          /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
          switch (d1') {
          | Cast(d1'', ty'', Hole) =>
            if (HTyp.eq(ty'', ty')) {
              BoxedValue(d1'');
            } else {
              Indet(FailedCast(d1', ty, ty'));
            }
          | _ =>
            JSUtil.log(DHExp.constructor_string(d1'));
            InvalidInput(6);
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          BoxedValue(Cast(d1', ty, ty'))
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* they might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result;
          } else {
            BoxedValue(Cast(d1', ty, ty'));
          }
        }
      | Indet(d1') as result =>
        switch (ground_cases_of(ty), ground_cases_of(ty')) {
        | (Hole, Hole) => result
        | (Ground, Ground) =>
          /* if two types are ground and consistent, then they are eq */
          result
        | (Ground, Hole) =>
          /* can't remove the cast or do anything else here, so we're done */
          Indet(Cast(d1', ty, ty'))
        | (Hole, Ground) =>
          switch (d1') {
          | Cast(d1'', ty'', Hole) =>
            if (HTyp.eq(ty'', ty')) {
              Indet(d1'');
            } else {
              Indet(FailedCast(d1', ty, ty'));
            }
          | _ => Indet(Cast(d1', ty, ty'))
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          Indet(Cast(d1', ty, ty'))
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* it might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result;
          } else {
            Indet(Cast(d1', ty, ty'));
          }
        }
      }
    | FailedCast(d1, ty, ty') =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(FailedCast(d1', ty, ty'))
      }
    }
  and evaluate_case =
      (scrut: DHExp.t, rules: list(DHExp.rule), current_rule_index: int)
      : result =>
    switch (evaluate(scrut)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(scrut)
    | Indet(scrut) =>
      switch (List.nth_opt(rules, current_rule_index)) {
      | None => Indet(Case(scrut, rules, current_rule_index))
      | Some(Rule(dp, d)) =>
        switch (DHExp.matches(dp, scrut)) {
        | Indet => Indet(Case(scrut, rules, current_rule_index))
        | Matches(env) => evaluate(DHExp.subst(env, d))
        | DoesNotMatch => evaluate_case(scrut, rules, current_rule_index + 1)
        }
      }
    };
};
