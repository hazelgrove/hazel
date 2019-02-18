open SemanticsCommon;

type nat = int;

type hole_sort =
  | ExpressionHole
  | PatternHole;

module Delta = {
  type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));
  let empty: t = (MetaVarMap.empty: t);
};

/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */
type inst_num = nat;

module DHPat = {
  type t =
    | EmptyHole(MetaVar.t, inst_num)
    | NonEmptyHole(in_hole_reason, MetaVar.t, inst_num, t)
    | Wild
    | Var(Var.t)
    | NumLit(nat)
    | BoolLit(bool)
    | Inj(inj_side, t)
    | ListNil
    | Cons(t, t)
    | Pair(t, t)
    | Triv /* unit intro */
    | Ap(t, t);

  let rec make_tuple = (ds: list(t)): t =>
    switch (ds) {
    | [d1, d2] => Pair(d1, d2)
    | [d1] => d1
    | [d1, ...ds] =>
      let d2 = make_tuple(ds);
      Pair(d1, d2);
    | [] => Triv
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
    | ListNil => false
    | Var(y) => Var.eq(x, y)
    | Inj(_, dp1) => binds_var(x, dp1)
    | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
    | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
    | Ap(dp1, dp2) => false
    };

  type expand_result =
    | Expands(t, HTyp.t, Contexts.t, Delta.t)
    | DoesNotExpand;

  let rec syn_expand =
          (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): expand_result =>
    switch (p) {
    | UHPat.Pat(InHole(TypeInconsistent as reason, u), p')
    | UHPat.Pat(
        InHole(WrongLength as reason, u),
        UHPat.OpSeq(
          Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _),
          _,
        ) as p',
      ) =>
      switch (syn_expand'(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (PatternHole, HTyp.Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, dp), HTyp.Hole, ctx, delta);
      }
    | UHPat.Pat(InHole(WrongLength, _), _) => DoesNotExpand
    | UHPat.Pat(NotInHole, p') => syn_expand'(ctx, delta, p')
    | UHPat.Parenthesized(p1) => syn_expand(ctx, delta, p1)
    }
  and syn_expand' =
      (ctx: Contexts.t, delta: Delta.t, p': UHPat.t'): expand_result =>
    switch (p') {
    | UHPat.EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | UHPat.Wild => Expands(Wild, HTyp.Hole, ctx, delta)
    | UHPat.Var(x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, HTyp.Hole));
      Expands(Var(x), HTyp.Hole, ctx, delta);
    | UHPat.NumLit(n) => Expands(NumLit(n), HTyp.Num, ctx, delta)
    | UHPat.BoolLit(b) => Expands(BoolLit(b), HTyp.Bool, ctx, delta)
    | UHPat.Inj(side, p) =>
      switch (syn_expand(ctx, delta, p)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let dp = Inj(side, dp1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, HTyp.Hole)
          | R => HTyp.Sum(HTyp.Hole, ty1)
          };
        Expands(dp, ty, ctx, delta);
      }
    | UHPat.ListNil => Expands(ListNil, HTyp.List(HTyp.Hole), ctx, delta)
    | UHPat.OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    }
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel_t, seq: UHPat.opseq)
      : expand_result =>
    switch (skel) {
    | Skel.Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(pn) => syn_expand(ctx, delta, pn)
      }
    | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | Skel.BinOp(
        InHole(WrongLength as reason, u),
        UHPat.Comma as op,
        skel1,
        skel2,
      ) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (PatternHole, HTyp.Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, dp), HTyp.Hole, ctx, delta);
      };
    | Skel.BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, ty2, ctx, delta) =>
          let dp = Pair(dp1, dp2);
          Expands(dp, HTyp.Prod(ty1, ty2), ctx, delta);
        }
      }
    | Skel.BinOp(NotInHole, UHPat.Space, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, ty2, ctx, delta) =>
          let dp = Ap(dp1, dp2);
          Expands(dp, HTyp.Hole, ctx, delta);
        }
      }
    | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
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
    | UHPat.Pat(NotInHole, p') => ana_expand'(ctx, delta, p', ty)
    | UHPat.Pat(InHole(TypeInconsistent as reason, u), p') =>
      switch (syn_expand'(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      }
    | UHPat.Pat(
        InHole(WrongLength as reason, u),
        UHPat.OpSeq(
          Skel.BinOp(InHole(WrongLength, _), UHPat.Comma, _, _),
          _,
        ) as p',
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
    | UHPat.Pat(InHole(WrongLength, _), _) => DoesNotExpand
    | UHPat.Parenthesized(p) => ana_expand(ctx, delta, p, ty)
    }
  and ana_expand' =
      (ctx: Contexts.t, delta: Delta.t, p': UHPat.t', ty: HTyp.t)
      : expand_result =>
    switch (p') {
    | UHPat.EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | UHPat.Var(x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      Expands(Var(x), ty, ctx, delta);
    | UHPat.Wild => Expands(Wild, ty, ctx, delta)
    | UHPat.NumLit(_)
    | UHPat.BoolLit(_) => syn_expand'(ctx, delta, p')
    | UHPat.Inj(side, p1) =>
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
    | UHPat.ListNil =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(ty_elt) => Expands(ListNil, HTyp.List(ty_elt), ctx, delta)
      }
    | UHPat.OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
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
    | Skel.Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(pn) => ana_expand(ctx, delta, pn, ty)
      }
    | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
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
    | Skel.BinOp(NotInHole, UHPat.Comma, skel1, skel2) =>
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
            Expands(dp, HTyp.Prod(ty1, ty2), ctx, delta);
          }
        }
      }
    | Skel.BinOp(InHole(WrongLength, u), UHPat.Comma, skel1, skel2) =>
      switch (ty) {
      | HTyp.Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHPat.get_tuple(skel1, skel2);
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let processed1 =
          List.fold_right(
            (skel_ty: (UHPat.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((elts, ctx, delta)) =>
                let (skel, ty) = skel_ty;
                switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
                | DoesNotExpand => None
                | Expands(dp, ty, ctx, delta) =>
                  Some(([(dp, ty), ...elts], ctx, delta))
                };
              },
            zipped,
            Some(([], ctx, delta)),
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
            let (ds, tys) = Util.unzip(elts1 @ elts2);
            let d = make_tuple(ds);
            let ty = HTyp.make_tuple(tys);
            Expands(d, ty, ctx, delta);
          };
        };
      | _ => DoesNotExpand
      }
    | Skel.BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | Skel.BinOp(NotInHole, UHPat.Space, skel1, skel2) => DoesNotExpand
    | Skel.BinOp(NotInHole, UHPat.Cons, skel1, skel2) =>
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

  let of_op = op =>
    switch (op) {
    | UHExp.Plus => Some((Plus, HTyp.Num))
    | UHExp.Times => Some((Times, HTyp.Num))
    | UHExp.LessThan => Some((LessThan, HTyp.Bool))
    | UHExp.Space
    | UHExp.Cons
    | UHExp.Comma => None
    };

  let to_op = bno =>
    switch (bno) {
    | Plus => UHExp.Plus
    | Times => UHExp.Times
    | LessThan => UHExp.LessThan
    };

  module DHExp = {
    type t =
      | EmptyHole(MetaVar.t, inst_num, VarMap.t_(t))
      | NonEmptyHole(in_hole_reason, MetaVar.t, inst_num, VarMap.t_(t), t)
      | FreeVar(MetaVar.t, inst_num, VarMap.t_(t), Var.t)
      | BoundVar(Var.t)
      | Let(DHPat.t, t, t)
      | FixF(Var.t, HTyp.t, t)
      | Lam(DHPat.t, HTyp.t, t)
      | Ap(t, t)
      | BoolLit(bool)
      | NumLit(nat)
      | BinNumOp(bin_num_op, t, t)
      | ListNil(HTyp.t)
      | Cons(t, t)
      | Inj(HTyp.t, inj_side, t)
      | Pair(t, t)
      | Triv
      | Case(t, list(rule), nat)
      | Cast(t, HTyp.t, HTyp.t)
      | FailedCast(t, HTyp.t, HTyp.t)
    and rule =
      | Rule(DHPat.t, t);
  };
  include DHExp;

  let rec constructor_string = d =>
    switch (d) {
    | EmptyHole(_, _, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _, _) => "NonEmptyHole"
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

  let rec make_tuple = (ds: list(t)): t =>
    switch (ds) {
    | [d1, d2] => Pair(d1, d2)
    | [d1] => d1
    | [d1, ...ds] =>
      let d2 = make_tuple(ds);
      Pair(d1, d2);
    | [] => Triv
    };

  let rec cast = (d: t, t1: HTyp.t, t2: HTyp.t): t =>
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
  and subst_var_rules = (d1: t, x: Var.t, rules: list(rule)) =>
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
  and subst_var_env = (d1: t, x: Var.t, sigma: Environment.t) =>
    List.map(
      xd => {
        let (y, d) = xd;
        (y, subst_var(d1, x, d));
      },
      sigma,
    );

  let rec subst = (env: Environment.t, d: t): t =>
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
    | (DHPat.EmptyHole(_, _), _)
    | (DHPat.NonEmptyHole(_, _, _, _), _) => Indet
    | (DHPat.Wild, _) => Matches(Environment.empty)
    | (DHPat.Var(x), _) =>
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
    | (DHPat.BoolLit(b1), BoolLit(b2)) =>
      if (b1 == b2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (DHPat.BoolLit(_), Cast(d, HTyp.Bool, HTyp.Hole)) => matches(dp, d)
    | (DHPat.BoolLit(_), Cast(d, HTyp.Hole, HTyp.Bool)) => matches(dp, d)
    | (DHPat.BoolLit(_), _) => DoesNotMatch
    | (DHPat.NumLit(n1), NumLit(n2)) =>
      if (n1 == n2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (DHPat.NumLit(_), Cast(d, HTyp.Num, HTyp.Hole)) => matches(dp, d)
    | (DHPat.NumLit(_), Cast(d, HTyp.Hole, HTyp.Num)) => matches(dp, d)
    | (DHPat.NumLit(_), _) => DoesNotMatch
    | (DHPat.Inj(side1, dp), Inj(_, side2, d)) =>
      switch (side1, side2) {
      | (L, L)
      | (R, R) => matches(dp, d)
      | _ => DoesNotMatch
      }
    | (
        DHPat.Inj(side, dp),
        Cast(d, HTyp.Sum(tyL1, tyR1), HTyp.Sum(tyL2, tyR2)),
      ) =>
      matches_cast_Inj(side, dp, d, [(tyL1, tyR1, tyL2, tyR2)])
    | (DHPat.Inj(_, _), Cast(d, HTyp.Sum(_, _), HTyp.Hole)) =>
      matches(dp, d)
    | (DHPat.Inj(_, _), Cast(d, HTyp.Hole, HTyp.Sum(_, _))) =>
      matches(dp, d)
    | (DHPat.Inj(_, _), _) => DoesNotMatch
    | (DHPat.Pair(dp1, dp2), Pair(d1, d2)) =>
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
    | (
        DHPat.Pair(dp1, dp2),
        Cast(d, HTyp.Prod(tyL1, tyR1), HTyp.Prod(tyL2, tyR2)),
      ) =>
      matches_cast_Pair(dp1, dp2, d, [(tyL1, tyL2)], [(tyR1, tyR2)])
    | (DHPat.Pair(dp1, dp2), Cast(d, HTyp.Hole, HTyp.Prod(_, _))) =>
      matches(dp, d)
    | (DHPat.Pair(dp1, dp2), Cast(d, HTyp.Prod(_, _), HTyp.Hole)) =>
      matches(dp, d)
    | (DHPat.Pair(_, _), _) => DoesNotMatch
    | (DHPat.Triv, Triv) => Matches(Environment.empty)
    | (DHPat.Triv, Cast(d, HTyp.Hole, HTyp.Unit)) => matches(dp, d)
    | (DHPat.Triv, Cast(d, HTyp.Unit, HTyp.Hole)) => matches(dp, d)
    | (DHPat.Triv, _) => DoesNotMatch
    | (DHPat.ListNil, ListNil(_)) => Matches(Environment.empty)
    | (DHPat.ListNil, Cast(d, HTyp.Hole, HTyp.List(_))) => matches(dp, d)
    | (DHPat.ListNil, Cast(d, HTyp.List(_), HTyp.Hole)) => matches(dp, d)
    | (DHPat.ListNil, Cast(d, HTyp.List(_), HTyp.List(_))) => matches(dp, d)
    | (DHPat.ListNil, _) => DoesNotMatch
    | (DHPat.Cons(dp1, dp2), Cons(d1, d2)) =>
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
    | (DHPat.Cons(dp1, dp2), Cast(d, HTyp.List(ty1), HTyp.List(ty2))) =>
      matches_cast_Cons(dp1, dp2, d, [(ty1, ty2)])
    | (DHPat.Cons(_, _), Cast(d, HTyp.Hole, HTyp.List(_))) =>
      matches(dp, d)
    | (DHPat.Cons(_, _), Cast(d, HTyp.List(_), HTyp.Hole)) =>
      matches(dp, d)
    | (DHPat.Cons(_, _), _) => DoesNotMatch
    | (DHPat.Ap(_, _), _) => DoesNotMatch
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
    | Cast(d', HTyp.Sum(tyL1, tyR1), HTyp.Sum(tyL2, tyR2)) =>
      matches_cast_Inj(side, dp, d', [(tyL1, tyR1, tyL2, tyR2), ...casts])
    | Cast(d', HTyp.Sum(_, _), HTyp.Hole)
    | Cast(d', HTyp.Hole, HTyp.Sum(_, _)) =>
      matches_cast_Inj(side, dp, d', casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
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
    | Cast(d', HTyp.Prod(tyL1, tyR1), HTyp.Prod(tyL2, tyR2)) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d',
        [(tyL1, tyL2), ...left_casts],
        [(tyR1, tyR2), ...right_casts],
      )
    | Cast(d', HTyp.Prod(_, _), HTyp.Hole)
    | Cast(d', HTyp.Hole, HTyp.Prod(_, _)) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
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
    | Cast(d', HTyp.List(ty1), HTyp.List(ty2)) =>
      matches_cast_Cons(dp1, dp2, d', [(ty1, ty2), ...elt_casts])
    | Cast(d', HTyp.List(_), HTyp.Hole) =>
      matches_cast_Cons(dp1, dp2, d', elt_casts)
    | Cast(d', HTyp.Hole, HTyp.List(_)) =>
      matches_cast_Cons(dp1, dp2, d', elt_casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
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

  /* Implementation of type assignment judgment in POPL 2019 paper.
    * Not actually called anywhere, now stale.
   Inductive type_result : Type =
   | WellTyped : HTyp.t -> type_result
   | IllTyped.

   Fixpoint assign_type
     (fuel : Fuel.t)
     (gamma : VarCtx.t) (delta : Delta.t)
     (d : t)
     : type_result =
       begin match with
       | Fuel.Kicked -> IllTyped
       | Fuel.More ->
       let assign_type = assign_type in
       begin match d with
       | BoundVar x ->
         begin match (Var.is_valid x, VarMap.lookup gamma x) with
         | (true, Some ty) -> WellTyped ty
         | _ -> IllTyped
         end
       | FreeVar u _ sigma x ->
         if (Var.is_valid x) then
           begin match MetaVarMap.lookup delta u with
           | Some (ty, gamma') ->
             if check_type_env gamma delta sigma gamma' then
               WellTyped ty
             else IllTyped
           | None -> IllTyped
           end
         else IllTyped
       |  d1 d2 ->
         begin match (Var.is_valid_binder x, assign_type gamma delta d1) with
         | (true, WellTyped ty1) ->
           let gamma' = VarMap.extend gamma (x, ty1) in
           assign_type gamma' delta d2
         | _ -> IllTyped
         end
       | FixF x ((HTyp.Arrow _ _) as ty1) d1 ->
         let gamma' = VarMap.extend gamma (x, ty1) in
         begin match (Var.is_valid_binder x, assign_type gamma' delta d1) with
         | (true, WellTyped ty2) ->
           if HTyp.eq ty1 ty2 then WellTyped ty2 else IllTyped
         | _ -> IllTyped
         end
       | FixF x _ d1 -> IllTyped
       | Lam x ty1 d1 ->
         let gamma' = VarMap.extend gamma (x, ty1) in
         begin match (Var.is_valid_binder x, assign_type gamma' delta d1) with
         | (true, WellTyped ty2) -> WellTyped (HTyp.Arrow ty1 ty2)
         | _ -> IllTyped
         end
       | Ap d1 d2 ->
         begin match assign_type gamma delta d1 with
         | IllTyped -> IllTyped
         | WellTyped (HTyp.Arrow ty2 ty) ->
           begin match assign_type gamma delta d2 with
           | IllTyped -> IllTyped
           | WellTyped ty2' ->
             if HTyp.eq ty2 ty2' then WellTyped ty
             else IllTyped
           end
         | WellTyped _ -> IllTyped
         end
       | NumLit _ -> WellTyped HTyp.Num
       | BinNumOp _ d1 d2 ->
         begin match (assign_type gamma delta d1,
                assign_type gamma delta d2) with
         | (WellTyped HTyp.Num, WellTyped HTyp.Num) ->
           WellTyped HTyp.Num
         | _ -> IllTyped
         end
       | Inj other_ty side d1 ->
         begin match assign_type gamma delta d1 with
         | IllTyped -> IllTyped
         | WellTyped ty1 ->
           begin match side with
           | L -> WellTyped (HTyp.Sum ty1 other_ty)
           | R -> WellTyped (HTyp.Sum other_ty ty1)
           end
         end
       | Case d1 (x, d2) (y, d3) ->
         begin match ((Var.is_valid_binder x) && (Var.is_valid_binder y), assign_type gamma delta d1) with
         | (true, WellTyped (HTyp.Sum tyL tyR)) ->
           let gamma1 = VarMap.extend gamma (x, tyL) in
           let gamma2 = VarMap.extend gamma (y, tyR) in
           begin match (assign_type gamma1 delta d2,
                  assign_type gamma2 delta d3) with
           | (WellTyped ty2, WellTyped ty3) ->
             if HTyp.eq ty2 ty3 then WellTyped ty2
             else IllTyped
           | _ -> IllTyped
           end
         | _ -> IllTyped
         end
       | EmptyHole u _ sigma ->
         begin match MetaVarMap.lookup delta u with
         | Some (ty, gamma') ->
           if check_type_env gamma delta sigma gamma' then
             WellTyped ty
           else IllTyped
         | None -> IllTyped
         end
       | NonEmptyHole reason u _ sigma d1 ->
         begin match assign_type gamma delta d1 with
         | WellTyped _ ->
           begin match MetaVarMap.lookup delta u with
           | Some (ty, gamma') ->
             if check_type_env gamma delta sigma gamma' then
               WellTyped ty
             else IllTyped
           | None -> IllTyped
           end
         | IllTyped -> IllTyped
         end
       | Cast d1 ty1 ty2
       | FailedCast d1 ty1 ty2 ->
         begin match assign_type gamma delta d1 with
         | IllTyped -> IllTyped
         | WellTyped ty1' ->
           if HTyp.eq ty1 ty1' &&
            HTyp.consistent ty1 ty2
           then WellTyped ty2
           else IllTyped
         end
       end
       end
   with check_type_env (fuel : Fuel.t)
           (gamma : VarCtx.t) (delta : Delta.t)
           (sigma : Environment.t)
           (gamma' : VarCtx.t) : bool =
       begin match with
       | Fuel.More ->
           Coq.Lists.List.forallb
             (fun xd : Var.t * t ->
               let (x, d) = xd in
               begin match assign_type gamma delta d with
               | WellTyped ty ->
                 begin match VarMap.lookup gamma' x with
                 | Some ty' -> HTyp.consistent ty ty'
                 | None -> false
                 end
               | IllTyped -> false
               end)
             sigma
       | Fuel.Kicked -> false
       end.
   */

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
    | UHExp.Parenthesized(e1) => syn_expand(ctx, delta, e1)
    | UHExp.Tm(NotInHole, e') => syn_expand'(ctx, delta, e')
    | UHExp.Tm(InHole(TypeInconsistent as reason, u), e')
    | UHExp.Tm(
        InHole(WrongLength as reason, u),
        UHExp.OpSeq(
          Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _),
          _,
        ) as e',
      ) =>
      switch (syn_expand'(ctx, delta, e')) {
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, HTyp.Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, sigma, d), HTyp.Hole, delta);
      | DoesNotExpand => DoesNotExpand
      }
    | UHExp.Tm(InHole(WrongLength, _), _) => DoesNotExpand
    }
  and syn_expand' =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t'): expand_result =>
    switch (e) {
    | UHExp.EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = DHExp.EmptyHole(u, 0, sigma);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | UHExp.Asc(e1, uty) =>
      let ty = UHTyp.expand(uty);
      switch (ana_expand(ctx, delta, e1, ty)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty', delta) => Expands(cast(d1, ty', ty), ty, delta)
      };
    | UHExp.Var(NotInVHole, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x)) {
      | Some(ty) => Expands(DHExp.BoundVar(x), ty, delta)
      | None => DoesNotExpand
      };
    | UHExp.Var(InVHole(u), x) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(
          delta,
          (u, (ExpressionHole, HTyp.Hole, gamma)),
        );
      Expands(DHExp.FreeVar(u, 0, sigma, x), HTyp.Hole, delta);
    | UHExp.Lam(p, ann, e1) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
      | DHPat.DoesNotExpand => DoesNotExpand
      | DHPat.Expands(dp, _, ctx, delta) =>
        switch (syn_expand(ctx, delta, e1)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty2, delta) =>
          let d = Lam(dp, ty1, d1);
          Expands(d, HTyp.Arrow(ty1, ty2), delta);
        }
      };
    | UHExp.LineItem(UHExp.EmptyLine, e1)
    | UHExp.LineItem(UHExp.ExpLine(_), e1) => syn_expand(ctx, delta, e1)
    | UHExp.LineItem(UHExp.LetLine(p, ann, e1), e2) =>
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
          | DHPat.DoesNotExpand => DoesNotExpand
          | DHPat.Expands(dp, _, ctx, delta) =>
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
          | DHPat.DoesNotExpand => DoesNotExpand
          | DHPat.Expands(dp, _, ctx, delta) =>
            switch (syn_expand(ctx, delta, e2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, delta2) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | UHExp.NumLit(n) => Expands(NumLit(n), HTyp.Num, delta)
    | UHExp.BoolLit(b) => Expands(BoolLit(b), HTyp.Bool, delta)
    | UHExp.Inj(side, e1) =>
      switch (syn_expand(ctx, delta, e1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let d = DHExp.Inj(HTyp.Hole, side, d1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, HTyp.Hole)
          | R => HTyp.Sum(HTyp.Hole, ty1)
          };
        Expands(d, ty, delta);
      }
    | UHExp.ListNil =>
      let elt_ty = HTyp.Hole;
      Expands(ListNil(elt_ty), HTyp.List(elt_ty), delta);
    | UHExp.Case(_, _) => DoesNotExpand
    | UHExp.OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    | UHExp.ApPalette(name, serialized_model, hole_data) => DoesNotExpand
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
    | Skel.Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => syn_expand(ctx, delta, en)
      }
    | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | Skel.BinOp(
        InHole(WrongLength as reason, u),
        UHExp.Comma as op,
        skel1,
        skel2,
      ) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, HTyp.Hole, gamma)),
          );
        Expands(NonEmptyHole(reason, u, 0, sigma, d), HTyp.Hole, delta);
      };
    | Skel.BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | Skel.BinOp(NotInHole, UHExp.Space, skel1, skel2) =>
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
    | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
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
    | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
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
    | Skel.BinOp(NotInHole, UHExp.Plus as op, skel1, skel2)
    | Skel.BinOp(NotInHole, UHExp.Times as op, skel1, skel2)
    | Skel.BinOp(NotInHole, UHExp.LessThan as op, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, HTyp.Num)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, HTyp.Num)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = cast(d1, ty1, HTyp.Num);
          let dc2 = cast(d2, ty2, HTyp.Num);
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
    | UHExp.Tm(NotInHole, e') => ana_expand'(ctx, delta, e', ty)
    | UHExp.Tm(InHole(TypeInconsistent as reason, u), e') =>
      switch (syn_expand'(ctx, delta, e')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        Expands(NonEmptyHole(reason, u, 0, sigma, d), ty, delta);
      }
    | UHExp.Tm(
        InHole(WrongLength as reason, u),
        UHExp.OpSeq(
          Skel.BinOp(InHole(WrongLength, _), UHExp.Comma, _, _),
          _,
        ) as e',
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
    | UHExp.Tm(InHole(WrongLength, _), _) => DoesNotExpand
    | UHExp.Parenthesized(e1) => ana_expand(ctx, delta, e1, ty)
    }
  and ana_expand' =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t', ty: HTyp.t)
      : expand_result =>
    switch (e) {
    | UHExp.EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = EmptyHole(u, 0, sigma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | UHExp.Var(InVHole(u), x) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(FreeVar(u, 0, sigma, x), ty, delta);
    | UHExp.LineItem(UHExp.EmptyLine, e1)
    | UHExp.LineItem(UHExp.ExpLine(_), e1) => syn_expand(ctx, delta, e1)
    | UHExp.LineItem(UHExp.LetLine(p, ann, e1), e2) =>
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
          | DHPat.DoesNotExpand => DoesNotExpand
          | DHPat.Expands(dp, _, ctx, delta) =>
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
          | DHPat.DoesNotExpand => DoesNotExpand
          | DHPat.Expands(dp, ty1, ctx, delta) =>
            switch (ana_expand(ctx, delta, e2, ty)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d2, ty, delta) =>
              let d = Let(dp, d1, d2);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | UHExp.Lam(p, ann, e1) =>
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
            | DHPat.DoesNotExpand => DoesNotExpand
            | DHPat.Expands(dp, ty1p, ctx, delta) =>
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
          | DHPat.DoesNotExpand => DoesNotExpand
          | DHPat.Expands(dp, ty1, ctx, delta) =>
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
    | UHExp.Inj(side, e1) =>
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
    | UHExp.ListNil =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(elt_ty) => Expands(ListNil(elt_ty), HTyp.List(elt_ty), delta)
      }
    | UHExp.Case(e1, rules) =>
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
    | UHExp.OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
    | UHExp.Asc(_, _)
    | UHExp.Var(NotInVHole, _)
    | UHExp.BoolLit(_)
    | UHExp.NumLit(_)
    | UHExp.ApPalette(_, _, _) =>
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
    | DHPat.DoesNotExpand => None
    | DHPat.Expands(dp, _, ctx, delta) =>
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
    | Skel.Placeholder(n) =>
      switch (OperatorSeq.seq_nth(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => ana_expand(ctx, delta, en, ty)
      }
    | Skel.BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
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
    | Skel.BinOp(NotInHole, UHExp.Comma, skel1, skel2) =>
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
            Expands(d, HTyp.Prod(ty1, ty2), delta);
          }
        }
      }
    | Skel.BinOp(InHole(WrongLength, u), UHExp.Comma, skel1, skel2) =>
      switch (ty) {
      | HTyp.Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let processed1 =
          List.fold_right(
            (skel_ty: (UHExp.skel_t, HTyp.t), opt_result) =>
              switch (opt_result) {
              | None => None
              | Some((elts, delta)) =>
                let (skel, ty) = skel_ty;
                switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
                | DoesNotExpand => None
                | Expands(d, ty, delta) =>
                  Some(([(d, ty), ...elts], delta))
                };
              },
            zipped,
            Some(([], delta)),
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
            let (ds, tys) = Util.unzip(elts1 @ elts2);
            let d = make_tuple(ds);
            let ty = HTyp.make_tuple(tys);
            Expands(d, ty, delta);
          };
        };
      | _ => DoesNotExpand
      }
    | Skel.BinOp(InHole(WrongLength, _), _, _, _) => DoesNotExpand
    | Skel.BinOp(NotInHole, UHExp.Cons, skel1, skel2) =>
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
    | Skel.BinOp(_, UHExp.Plus, _, _)
    | Skel.BinOp(_, UHExp.Times, _, _)
    | Skel.BinOp(_, UHExp.LessThan, _, _)
    | Skel.BinOp(_, UHExp.Space, _, _) =>
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
        : (nat, t) => {
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
            Util.update_nth(
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

    let num_instances = (hii: t, u: MetaVar.t) =>
      switch (MetaVarMap.lookup(hii, u)) {
      | Some(envs) => List.length(envs)
      | None => 0
      };

    let default_instance = (hii: t, u: MetaVar.t) =>
      switch (MetaVarMap.lookup(hii, u)) {
      | Some(envs) =>
        switch (envs) {
        | [] => None
        | [_, ..._] => Some((u, 0))
        }
      | None => None
      };

    let lookup = (hii: t, inst: HoleInstance.t) => {
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
    | DHPat.Wild
    | DHPat.Var(_)
    | DHPat.NumLit(_)
    | DHPat.BoolLit(_)
    | DHPat.ListNil
    | DHPat.Triv => (dp, hii)
    | DHPat.EmptyHole(u, _) =>
      /* TODO: Pattern holes don't need environments. Maybe this calls
       * for a refactoring of types to reflect this, e.g., a separate
       * PatHoleInstance type. Passing in an empty environment for now. */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (DHPat.EmptyHole(u, i), hii);
    | DHPat.NonEmptyHole(reason, u, _, dp1) =>
      /* TODO: see above */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      (DHPat.NonEmptyHole(reason, u, i, dp1), hii);
    | DHPat.Inj(side, dp1) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      (DHPat.Inj(side, dp1), hii);
    | DHPat.Pair(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (DHPat.Pair(dp1, dp2), hii);
    | DHPat.Cons(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (DHPat.Cons(dp1, dp2), hii);
    | DHPat.Ap(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only_pat(path, hii, dp1);
      let (dp2, hii) = renumber_result_only_pat(path, hii, dp2);
      (DHPat.Pair(dp1, dp2), hii);
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
    | InvalidInput(nat)
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

  let grounded_Arrow = NotGroundOrHole(HTyp.Arrow(HTyp.Hole, HTyp.Hole));
  let grounded_Sum = NotGroundOrHole(HTyp.Sum(HTyp.Hole, HTyp.Hole));
  let grounded_Prod = NotGroundOrHole(HTyp.Prod(HTyp.Hole, HTyp.Hole));
  let grounded_List = NotGroundOrHole(HTyp.List(HTyp.Hole));

  let ground_cases_of = ty =>
    switch (ty) {
    | HTyp.Hole => Hole
    | HTyp.Bool
    | HTyp.Num
    | HTyp.Unit
    | HTyp.Arrow(HTyp.Hole, HTyp.Hole)
    | HTyp.Sum(HTyp.Hole, HTyp.Hole)
    | HTyp.Prod(HTyp.Hole, HTyp.Hole)
    | HTyp.List(HTyp.Hole) => Ground
    | HTyp.Arrow(_, _) => grounded_Arrow
    | HTyp.Sum(_, _) => grounded_Sum
    | HTyp.Prod(_, _) => grounded_Prod
    | HTyp.List(_) => grounded_List
    };

  let eval_bin_num_op = (op, n1, n2) =>
    switch (op) {
    | DHExp.Plus => DHExp.NumLit(n1 + n2)
    | DHExp.Times => DHExp.NumLit(n1 * n2)
    | DHExp.LessThan => DHExp.BoolLit(n1 < n2)
    };

  let rec evaluate = (d: DHExp.t): result =>
    switch (d) {
    | DHExp.BoundVar(_) => InvalidInput(1)
    | DHExp.Let(dp, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (DHExp.matches(dp, d1)) {
        | DHExp.Indet => Indet(d)
        | DHExp.DoesNotMatch => Indet(d)
        | DHExp.Matches(env) => evaluate(DHExp.subst(env, d2))
        }
      }
    | DHExp.FixF(x, ty, d1) => evaluate(DHExp.subst_var(d, x, d1))
    | DHExp.Lam(_, _, _) => BoxedValue(d)
    | DHExp.Ap(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(DHExp.Lam(dp, tau, d3)) =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2)
        | Indet(d2) =>
          switch (DHExp.matches(dp, d2)) {
          | DHExp.DoesNotMatch => Indet(d)
          | DHExp.Indet => Indet(d)
          | DHExp.Matches(env) =>
            /* beta rule */
            evaluate(DHExp.subst(env, d3))
          }
        }
      | BoxedValue(
          DHExp.Cast(d1', HTyp.Arrow(ty1, ty2), HTyp.Arrow(ty1', ty2')),
        )
      | Indet(
          DHExp.Cast(d1', HTyp.Arrow(ty1, ty2), HTyp.Arrow(ty1', ty2')),
        ) =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') =>
          /* ap cast rule */
          evaluate(
            DHExp.Cast(
              DHExp.Ap(d1', DHExp.Cast(d2', ty1', ty1)),
              ty2,
              ty2',
            ),
          )
        }
      | BoxedValue(_) => InvalidInput(2)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(DHExp.Ap(d1', d2'))
        }
      }
    | DHExp.ListNil(_)
    | DHExp.BoolLit(_)
    | DHExp.NumLit(_)
    | DHExp.Triv => BoxedValue(d)
    | DHExp.BinNumOp(op, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(DHExp.NumLit(n1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(DHExp.NumLit(n2)) =>
          BoxedValue(eval_bin_num_op(op, n1, n2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(DHExp.BinNumOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(DHExp.BinNumOp(op, d1', d2'))
        }
      }
    | DHExp.Inj(ty, side, d1) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1') => BoxedValue(DHExp.Inj(ty, side, d1'))
      | Indet(d1') => Indet(DHExp.Inj(ty, side, d1'))
      }
    | DHExp.Pair(d1, d2) =>
      switch (evaluate(d1), evaluate(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(DHExp.Pair(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(DHExp.Pair(d1, d2))
      }
    | DHExp.Cons(d1, d2) =>
      switch (evaluate(d1), evaluate(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(DHExp.Cons(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(DHExp.Cons(d1, d2))
      }
    | DHExp.Case(d1, rules, n) => evaluate_case(d1, rules, n)
    | DHExp.EmptyHole(u, i, sigma) => Indet(d)
    | DHExp.NonEmptyHole(reason, u, i, sigma, d1) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(DHExp.NonEmptyHole(reason, u, i, sigma, d1'))
      }
    | DHExp.FreeVar(u, i, sigma, x) => Indet(d)
    | DHExp.Cast(d1, ty, ty') =>
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
          BoxedValue(DHExp.Cast(d1', ty, ty'))
        | (Hole, Ground) =>
          /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
          switch (d1') {
          | DHExp.Cast(d1'', ty'', HTyp.Hole) =>
            if (HTyp.eq(ty'', ty')) {
              BoxedValue(d1'');
            } else {
              Indet(DHExp.FailedCast(d1', ty, ty'));
            }
          | _ =>
            JSUtil.log(DHExp.constructor_string(d1'));
            InvalidInput(6);
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(DHExp.Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' =
            DHExp.Cast(DHExp.Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          BoxedValue(DHExp.Cast(d1', ty, ty'))
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* they might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result;
          } else {
            BoxedValue(DHExp.Cast(d1', ty, ty'));
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
          Indet(DHExp.Cast(d1', ty, ty'))
        | (Hole, Ground) =>
          switch (d1') {
          | DHExp.Cast(d1'', ty'', HTyp.Hole) =>
            if (HTyp.eq(ty'', ty')) {
              Indet(d1'');
            } else {
              Indet(DHExp.FailedCast(d1', ty, ty'));
            }
          | _ => Indet(DHExp.Cast(d1', ty, ty'))
          }
        | (Hole, NotGroundOrHole(ty'_grounded)) =>
          /* ITExpand rule */
          let d' =
            DHExp.Cast(DHExp.Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
          evaluate(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' =
            DHExp.Cast(DHExp.Cast(d1', ty, ty_grounded), ty_grounded, ty');
          evaluate(d');
        | (Ground, NotGroundOrHole(_))
        | (NotGroundOrHole(_), Ground) =>
          /* can't do anything when casting between diseq, non-hole types */
          Indet(DHExp.Cast(d1', ty, ty'))
        | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
          /* it might be eq in this case, so remove cast if so */
          if (HTyp.eq(ty, ty')) {
            result;
          } else {
            Indet(DHExp.Cast(d1', ty, ty'));
          }
        }
      }
    | DHExp.FailedCast(d1, ty, ty') =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(DHExp.FailedCast(d1', ty, ty'))
      }
    }
  and evaluate_case =
      (scrut: DHExp.t, rules: list(DHExp.rule), current_rule_index: nat)
      : result =>
    switch (evaluate(scrut)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(scrut)
    | Indet(scrut) =>
      switch (List.nth_opt(rules, current_rule_index)) {
      | None => Indet(DHExp.Case(scrut, rules, current_rule_index))
      | Some(DHExp.Rule(dp, d)) =>
        switch (DHExp.matches(dp, scrut)) {
        | DHExp.Indet => Indet(DHExp.Case(scrut, rules, current_rule_index))
        | DHExp.Matches(env) => evaluate(DHExp.subst(env, d))
        | DHExp.DoesNotMatch =>
          evaluate_case(scrut, rules, current_rule_index + 1)
        }
      }
    };
};