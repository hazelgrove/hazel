open GeneralUtil;
open Sexplib.Std;

type hole_sort =
  | ExpressionHole
  | PatternHole;

module Delta = {
  type t = MetaVarMap.t((hole_sort, HTyp.t, VarCtx.t));
  let empty: t = (MetaVarMap.empty: t);
} /* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */;

[@deriving sexp]
type inst_num = int;

module DHPat = {
  [@deriving sexp]
  type t =
    | EmptyHole(MetaVar.t, inst_num)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, inst_num, t)
    | Wild
    | Keyword(MetaVar.t, inst_num, Keyword.t)
    | Var(Var.t)
    | NumLit(int)
    | BoolLit(bool)
    | Inj(InjSide.t, t)
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
    } /* whether dp contains the variable x outside of a hole */;

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
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | NumLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let p' = UHPat.set_err_status_t(NotInHole, p);
      switch (syn_expand(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => DoesNotExpand
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | Wild(NotInHole) => Expands(Wild, Hole, ctx, delta)
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Expands(Keyword(u, 0, k), Hole, ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, Hole));
      Expands(Var(x), Hole, ctx, delta);
    | NumLit(NotInHole, n) => Expands(NumLit(n), Num, ctx, delta)
    | BoolLit(NotInHole, b) => Expands(BoolLit(b), Bool, ctx, delta)
    | ListNil(NotInHole) => Expands(ListNil, List(Hole), ctx, delta)
    | Parenthesized(p1) => syn_expand(ctx, delta, p1)
    | OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    | Inj(NotInHole, side, p) =>
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
    }
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel_t, seq: UHPat.opseq)
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.nth_tm(n, seq)) {
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
      | Expands(dp1, _, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(dp2, _, ctx, delta) =>
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
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | NumLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let p' = UHPat.set_err_status_t(NotInHole, p);
      switch (syn_expand(ctx, delta, p')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => DoesNotExpand
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = EmptyHole(u, 0);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Expands(dp, ty, ctx, delta);
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Expands(Keyword(u, 0, k), ty, ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      Expands(Var(x), ty, ctx, delta);
    | Wild(NotInHole) => Expands(Wild, ty, ctx, delta)
    | NumLit(NotInHole, _)
    | BoolLit(NotInHole, _) => syn_expand(ctx, delta, p)
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(ty_elt) => Expands(ListNil, HTyp.List(ty_elt), ctx, delta)
      }
    | Parenthesized(p) => ana_expand(ctx, delta, p, ty)
    | OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
    | Inj(NotInHole, side, p1) =>
      switch (HTyp.matched_sum(ty)) {
      | None => DoesNotExpand
      | Some((tyL, tyR)) =>
        let ty1 = InjSide.pick(side, tyL, tyR);
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
      switch (OperatorSeq.nth_tm(n, seq)) {
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
  [@deriving sexp]
  type bin_num_op =
    | Minus
    | Plus
    | Times
    | LessThan
    | GreaterThan
    | Equals;

  let of_op = (op: UHExp.op): option((bin_num_op, HTyp.t)) =>
    switch (op) {
    | Minus => Some((Minus, Num))
    | Plus => Some((Plus, Num))
    | Times => Some((Times, Num))
    | LessThan => Some((LessThan, Bool))
    | GreaterThan => Some((GreaterThan, Bool))
    | Equals => Some((Equals, Bool))
    | And
    | Or
    | Space
    | Cons
    | Comma => None
    };

  let to_op = (bno: bin_num_op): UHExp.op =>
    switch (bno) {
    | Minus => Minus
    | Plus => Plus
    | Times => Times
    | LessThan => LessThan
    | GreaterThan => GreaterThan
    | Equals => Equals
    };

  module DHExp = {
    [@deriving sexp]
    type t =
      | EmptyHole(MetaVar.t, inst_num, VarMap.t_(t))
      | NonEmptyHole(
          ErrStatus.HoleReason.t,
          MetaVar.t,
          inst_num,
          VarMap.t_(t),
          t,
        )
      | Keyword(MetaVar.t, inst_num, VarMap.t_(t), Keyword.t)
      | FreeVar(MetaVar.t, inst_num, VarMap.t_(t), Var.t)
      | BoundVar(Var.t)
      | Let(DHPat.t, t, t)
      | FixF(Var.t, HTyp.t, t)
      | Lam(DHPat.t, HTyp.t, t)
      | Ap(t, t)
      | BoolLit(bool)
      | NumLit(int)
      | BinNumOp(bin_num_op, t, t)
      | And(t, t)
      | Or(t, t)
      | ListNil(HTyp.t)
      | Cons(t, t)
      | Inj(HTyp.t, InjSide.t, t)
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
    | And(_, _) => "And"
    | Or(_, _) => "Or"
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
    [@deriving sexp]
    type t = VarMap.t_(DHExp.t);
    include VarMap;
  } /* closed substitution [d1/x]d2*/;

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
    | And(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      And(d3, d4);
    | Or(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Or(d3, d4);
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
    | (_, BinNumOp(_, _, _) | And(_, _) | Or(_, _)) => Indet
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
        side: InjSide.t,
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
    | BinNumOp(_, _, _)
    | And(_, _)
    | Or(_, _) => Indet
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
    | BinNumOp(_, _, _)
    | And(_, _)
    | Or(_, _) => Indet
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
    | BinNumOp(_, _, _)
    | And(_, _)
    | Or(_, _) => Indet
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

  type expand_result_lines =
    | LinesExpand(t => t, Contexts.t, Delta.t)
    | LinesDoNotExpand;

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

  let rec syn_expand_block =
          (ctx: Contexts.t, delta: Delta.t, block: UHExp.block): expand_result =>
    switch (block) {
    | Block(lines, e) =>
      switch (syn_expand_lines(ctx, delta, lines)) {
      | LinesDoNotExpand => DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (syn_expand_exp(ctx, delta, e)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and syn_expand_lines =
      (ctx: Contexts.t, delta: Delta.t, lines: UHExp.lines)
      : expand_result_lines =>
    switch (lines) {
    | [] => LinesExpand(d => d, ctx, delta)
    | [line, ...lines] =>
      switch (syn_expand_line(ctx, delta, line)) {
      | LinesDoNotExpand => LinesDoNotExpand
      | LinesExpand(prelude_line, ctx, delta) =>
        switch (syn_expand_lines(ctx, delta, lines)) {
        | LinesDoNotExpand => LinesDoNotExpand
        | LinesExpand(prelude_lines, ctx, delta) =>
          LinesExpand(d => prelude_line(prelude_lines(d)), ctx, delta)
        }
      }
    }
  and syn_expand_line =
      (ctx: Contexts.t, delta: Delta.t, line: UHExp.line): expand_result_lines =>
    switch (line) {
    | ExpLine(e1) =>
      switch (syn_expand_exp(ctx, delta, e1)) {
      | DoesNotExpand => LinesDoNotExpand
      | Expands(d1, _, delta) =>
        let prelude = d2 => DHExp.Let(Wild, d1, d2);
        LinesExpand(prelude, ctx, delta);
      }
    | EmptyLine => LinesExpand(d => d, ctx, delta)
    | LetLine(p, ann, block) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let (ctx1, is_recursive_fn) =
          Statics.ctx_for_let'(ctx, p, ty1, block);
        switch (ana_expand_block(ctx1, delta, block, ty1)) {
        | DoesNotExpand => LinesDoNotExpand
        | Expands(d1, ty1', delta) =>
          let d1 =
            switch (is_recursive_fn) {
            | None => d1
            | Some(x) =>
              FixF(x, ty1', subst_var(cast(BoundVar(x), ty1', ty1), x, d1))
            };
          let d1 = cast(d1, ty1', ty1);
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => LinesDoNotExpand
          | Expands(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesExpand(prelude, ctx, delta);
          };
        };
      | None =>
        switch (syn_expand_block(ctx, delta, block)) {
        | DoesNotExpand => LinesDoNotExpand
        | Expands(d1, ty1, delta) =>
          switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
          | DoesNotExpand => LinesDoNotExpand
          | Expands(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesExpand(prelude, ctx, delta);
          }
        }
      }
    }
  and syn_expand_exp =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): expand_result =>
    switch (e) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | NumLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(InHole(TypeInconsistent as reason, u), _, _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let e' = UHExp.set_err_status_t(NotInHole, e);
      switch (syn_expand_exp(ctx, delta, e')) {
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
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(InHole(WrongLength, _), _, _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) =>
      DoesNotExpand /* not in hole */
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = DHExp.EmptyHole(u, 0, sigma);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | Var(NotInHole, NotInVarHole, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x)) {
      | Some(ty) => Expands(BoundVar(x), ty, delta)
      | None => DoesNotExpand
      };
    | Var(NotInHole, InVarHole(reason, u), x) =>
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
    | NumLit(NotInHole, n) => Expands(NumLit(n), Num, delta)
    | BoolLit(NotInHole, b) => Expands(BoolLit(b), Bool, delta)
    | ListNil(NotInHole) =>
      let elt_ty = HTyp.Hole;
      Expands(ListNil(elt_ty), List(elt_ty), delta);
    | Parenthesized(block) => syn_expand_block(ctx, delta, block)
    | OpSeq(skel, seq) => syn_expand_skel(ctx, delta, skel, seq)
    | Lam(NotInHole, p, ann, block) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (DHPat.ana_expand(ctx, delta, p, ty1)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        switch (syn_expand_block(ctx, delta, block)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d1, ty2, delta) =>
          let d = Lam(dp, ty1, d1);
          Expands(d, Arrow(ty1, ty2), delta);
        }
      };
    | Inj(NotInHole, side, block) =>
      switch (syn_expand_block(ctx, delta, block)) {
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
    | Case(NotInHole, block, rules, Some(uty)) =>
      let ty = UHTyp.expand(uty);
      switch (syn_expand_block(ctx, delta, block)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(ctx, delta, rules, ty1, ty)) {
        | None => DoesNotExpand
        | Some((drs, delta)) =>
          let d = Case(d1, drs, 0);
          Expands(d, ty, delta);
        }
      };
    | Case(NotInHole, _, _, None) => DoesNotExpand
    | ApPalette(NotInHole, _name, _serialized_model, _hole_data) =>
      DoesNotExpand /* let (_, palette_ctx) = ctx in   begin match (VarMap.lookup palette_ctx name) with   | Some palette_defn ->     let expansion_ty = UHExp.PaletteDefinition.expansion_ty palette_defn in     let to_exp = UHExp.PaletteDefinition.to_exp palette_defn in     let expansion = to_exp serialized_model in     let (_, hole_map) = hole_data in     (* bind each free variable in expansion by wrapping expansion      * in lambda, then apply lambda to args in hole data      *)     let bound_expansion :=         NatMap.fold hole_map           (fun bound entry ->             let (n, typ_exp) = entry in             let (htyp, hexp) = typ_exp in             let lam = UHExp.Tm NotInHole (UHExp.Lam (UHExp.PaletteHoleData.mk_hole_ref_var_name n) bound) in             let hexp_ann = UHExp.Tm NotInHole (UHExp.Asc (UHExp.Parenthesized hexp) (UHTyp.contract htyp)) in             let opseq = OperatorSeq.ExpOpExp (UHExp.Parenthesized lam) UHExp.Space (UHExp.Parenthesized hexp_ann) in             let ap = UHExp.OpSeq (Associator.associate_exp opseq) opseq in             UHExp.Tm NotInHole ap           )           expansion in     ana_expand_exp ctx bound_expansion expansion_ty   | None -> DoesNotExpand   end */ /* TODO fix me */
    }
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHExp.skel_t, seq: UHExp.opseq)
      : expand_result =>
    switch (skel) {
    | Placeholder(n) =>
      switch (OperatorSeq.nth_tm(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => syn_expand_exp(ctx, delta, en)
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
    | BinOp(NotInHole, Minus as op, skel1, skel2)
    | BinOp(NotInHole, Plus as op, skel1, skel2)
    | BinOp(NotInHole, Times as op, skel1, skel2)
    | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
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
    | BinOp(NotInHole, (And | Or) as op, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Bool)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Bool)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = cast(d1, ty1, Bool);
          let dc2 = cast(d2, ty2, Bool);
          switch (of_op(op)) {
          | None => DoesNotExpand
          | Some((op, ty)) =>
            let d = BinNumOp(op, dc1, dc2);
            Expands(d, ty, delta);
          };
        }
      }
    }
  and ana_expand_block =
      (ctx: Contexts.t, delta: Delta.t, block: UHExp.block, ty: HTyp.t)
      : expand_result =>
    switch (block) {
    | Block(lines, e) =>
      switch (syn_expand_lines(ctx, delta, lines)) {
      | LinesDoNotExpand => DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (ana_expand_exp(ctx, delta, e, ty)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and ana_expand_exp =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t, ty: HTyp.t): expand_result =>
    switch (e) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | NumLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(InHole(TypeInconsistent as reason, u), _, _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let e' = UHExp.set_err_status_t(NotInHole, e);
      switch (syn_expand_exp(ctx, delta, e')) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        Expands(NonEmptyHole(reason, u, 0, sigma, d), ty, delta);
      };
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(InHole(WrongLength, _), _, _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) =>
      DoesNotExpand /* not in hole */
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = EmptyHole(u, 0, sigma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | Var(NotInHole, InVarHole(reason, u), x) =>
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
    | Parenthesized(block) => ana_expand_block(ctx, delta, block, ty)
    | OpSeq(skel, seq) => ana_expand_skel(ctx, delta, skel, seq, ty)
    | Lam(NotInHole, p, ann, block) =>
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
              switch (ana_expand_block(ctx, delta, block, ty2)) {
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
            switch (ana_expand_block(ctx, delta, block, ty2)) {
            | DoesNotExpand => DoesNotExpand
            | Expands(d1, ty2, delta) =>
              let ty = HTyp.Arrow(ty1, ty2);
              let d = Lam(dp, ty1, d1);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | Inj(NotInHole, side, block) =>
      switch (HTyp.matched_sum(ty)) {
      | None => DoesNotExpand
      | Some((ty1, ty2)) =>
        let e1ty = InjSide.pick(side, ty1, ty2);
        switch (ana_expand_block(ctx, delta, block, e1ty)) {
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
    | Case(NotInHole, block, rules, Some(uty)) =>
      let ty2 = UHTyp.expand(uty);
      switch (HTyp.consistent(ty, ty2)) {
      | false => DoesNotExpand
      | true =>
        switch (syn_expand_block(ctx, delta, block)) {
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
    | Case(NotInHole, block, rules, None) =>
      switch (syn_expand_block(ctx, delta, block)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(ctx, delta, rules, ty1, ty)) {
        | None => DoesNotExpand
        | Some((drs, delta)) =>
          let d = Case(d1, drs, 0);
          Expands(d, ty, delta);
        }
      }
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => DoesNotExpand
      | Some(elt_ty) => Expands(ListNil(elt_ty), List(elt_ty), delta)
      }
    | Var(NotInHole, NotInVarHole, _)
    | BoolLit(NotInHole, _)
    | NumLit(NotInHole, _)
    | ApPalette(NotInHole, _, _, _) =>
      /* subsumption */
      syn_expand_exp(ctx, delta, e)
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
    let UHExp.Rule(p, block) = r;
    switch (DHPat.ana_expand(ctx, delta, p, pat_ty)) {
    | DoesNotExpand => None
    | Expands(dp, _, ctx, delta) =>
      switch (ana_expand_block(ctx, delta, block, clause_ty)) {
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
      switch (OperatorSeq.nth_tm(n, seq)) {
      | None => DoesNotExpand
      | Some(en) => ana_expand_exp(ctx, delta, en, ty)
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
    | BinOp(_, Minus | And | Or, _, _)
    | BinOp(_, Plus, _, _)
    | BinOp(_, Times, _, _)
    | BinOp(_, LessThan | GreaterThan | Equals, _, _)
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
    [@deriving sexp]
    type t = (MetaVar.t, inst_num);
  };

  module InstancePath = {
    [@deriving sexp]
    type t = list((HoleInstance.t, Var.t));
  };

  module HoleInstanceInfo = {
    [@deriving sexp]
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
            GeneralUtil.update_nth(
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
    | And(d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (And(d1, d2), hii);
    | Or(d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (Or(d1, d2), hii);
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
    | And(d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (And(d1, d2), hii);
    | Or(d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (Or(d1, d2), hii);
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
  [@deriving sexp]
  type result =
    | InvalidInput(int)
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t) /*    0 = out of fuel    1 = free or invalid variable    2 = ap invalid boxed function val    3 = boxed value not a number literal 2    4 = boxed value not a number literal 1    5 = bad pattern match    6 = Cast BV Hole Ground  */;

  [@deriving sexp]
  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

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
    | Minus => NumLit(n1 - n2)
    | Plus => NumLit(n1 + n2)
    | Times => NumLit(n1 * n2)
    | LessThan => BoolLit(n1 < n2)
    | GreaterThan => BoolLit(n1 > n2)
    | Equals => BoolLit(n1 == n2)
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
    | FixF(x, _, d1) => evaluate(DHExp.subst_var(d, x, d1))
    | Lam(_, _, _) => BoxedValue(d)
    | Ap(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(Lam(dp, _, d3)) =>
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
    | And(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(BoolLit(b2)) => BoxedValue(BoolLit(b1 && b2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(And(d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(And(d1', d2'))
        }
      }
    | Or(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(BoolLit(b2)) => BoxedValue(BoolLit(b1 || b2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(Or(d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(Or(d1', d2'))
        }
      }
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
            // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
            InvalidInput(6)
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
