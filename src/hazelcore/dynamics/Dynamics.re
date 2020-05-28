open Sexplib.Std;

/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */

module Pat = {
  module ExpandResult = {
    type t =
      | Expands(DHPat.t, HTyp.t, Contexts.t, Delta.t)
      | DoesNotExpand;

    let to_option =
      fun
      | DoesNotExpand => None
      | Expands(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

    let from_option =
      fun
      | None => DoesNotExpand
      | Some((pat, ty, ctx, delta)) => Expands(pat, ty, ctx, delta);

    let bind = (x: t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t): t =>
      switch (x) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
      };
  };

  module Let_syntax = ExpandResult;

  let rec syn_expand =
          (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): ExpandResult.t =>
    syn_expand_opseq(ctx, delta, p)
  and syn_expand_opseq =
      (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
      : ExpandResult.t =>
    syn_expand_skel(ctx, delta, skel, seq)
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel, seq: UHPat.seq)
      : ExpandResult.t =>
    switch (skel) {
    | Placeholder(n) =>
      syn_expand_operand(ctx, delta, seq |> Seq.nth_operand(n))
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => ExpandResult.DoesNotExpand
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHPat.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, ctx, delta) =
          syn_expand_skel(ctx, delta, skel1, seq);
        let%bind (dp2, ty2, ctx, delta) =
          syn_expand_skel(ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, ctx, delta), skel) => {
               syn_expand_skel(ctx, delta, skel, seq)
               |> ExpandResult.to_option
               |> Option.map(((dp, ty, ctx, delta)) =>
                    ((DHPat.Pair(dp_acc, dp), ctx, delta), ty)
                  )
             },
             (DHPat.Pair(dp1, dp2), ctx, delta),
           )
        |> Option.map((((dp_acc, ctx, delta), tys)) =>
             (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), ctx, delta)
           )
        |> ExpandResult.from_option;
      | _ =>
        raise(
          Invalid_argument(
            "Encountered tuple pattern type with less than 2 elements!",
          ),
        )
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        switch (syn_expand_skel(ctx, delta, skel2, seq)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp2, _, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Expands(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_expand_skel(ctx, delta, skel2, seq, ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp2, _, ctx, delta) =>
          let dp = DHPat.Cons(dp1, dp2);
          Expands(dp, ty, ctx, delta);
        };
      }
    }
  and syn_expand_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand)
      : ExpandResult.t =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(ctx, delta, operand')) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Expands(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => ExpandResult.DoesNotExpand
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = DHPat.EmptyHole(u, 0);
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
    | IntLit(NotInHole, n) =>
      switch (int_of_string_opt(n)) {
      | Some(n) => Expands(IntLit(n), Int, ctx, delta)
      | None => DoesNotExpand
      }
    | FloatLit(NotInHole, f) =>
      switch (TextShape.hazel_float_of_string_opt(f)) {
      | Some(f) => Expands(FloatLit(f), Float, ctx, delta)
      | None => DoesNotExpand
      }
    | BoolLit(NotInHole, b) => Expands(BoolLit(b), Bool, ctx, delta)
    | ListNil(NotInHole) => Expands(ListNil, List(Hole), ctx, delta)
    | Parenthesized(p1) => syn_expand(ctx, delta, p1)
    | Inj(NotInHole, side, p) =>
      switch (syn_expand(ctx, delta, p)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, ty1, ctx, delta) =>
        let dp = DHPat.Inj(side, dp1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Expands(dp, ty, ctx, delta);
      }
    }
  and ana_expand =
      (ctx: Contexts.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t)
      : ExpandResult.t =>
    ana_expand_opseq(ctx, delta, p, ty)
  and ana_expand_opseq =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        OpSeq(skel, seq) as opseq: UHPat.opseq,
        ty: HTyp.t,
      )
      : ExpandResult.t => {
    // handle n-tuples
    switch (Statics.Pat.tuple_zip(skel, ty)) {
    | Some(skel_tys) =>
      skel_tys
      |> List.fold_left(
           (
             acc: option((list(DHPat.t), Contexts.t, Delta.t)),
             (skel: UHPat.skel, ty: HTyp.t),
           ) =>
             switch (acc) {
             | None => None
             | Some((rev_dps, ctx, delta)) =>
               switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
               | ExpandResult.DoesNotExpand => None
               | Expands(dp, _, ctx, delta) =>
                 Some(([dp, ...rev_dps], ctx, delta))
               }
             },
           Some(([], ctx, delta)),
         )
      |> (
        fun
        | None => ExpandResult.DoesNotExpand
        | Some((rev_dps, ctx, delta)) => {
            let dp = rev_dps |> List.rev |> DHPat.mk_tuple;
            Expands(dp, ty, ctx, delta);
          }
      )
    | None =>
      if (List.length(HTyp.get_prod_elements(ty)) == 1) {
        skel
        |> UHPat.get_tuple_elements
        |> List.fold_left(
             (
               acc: option((list(DHPat.t), Contexts.t, Delta.t)),
               skel: UHPat.skel,
             ) =>
               switch (acc) {
               | None => None
               | Some((rev_dps, ctx, delta)) =>
                 switch (syn_expand_skel(ctx, delta, skel, seq)) {
                 | DoesNotExpand => None
                 | Expands(dp, _, ctx, delta) =>
                   Some(([dp, ...rev_dps], ctx, delta))
                 }
               },
             Some(([], ctx, delta)),
           )
        |> (
          fun
          | None => ExpandResult.DoesNotExpand
          | Some((rev_dps, ctx, delta)) => {
              let dp = DHPat.mk_tuple(List.rev(rev_dps));
              Expands(dp, ty, ctx, delta);
            }
        );
      } else {
        switch (opseq |> UHPat.get_err_status_opseq) {
        | NotInHole
        | InHole(TypeInconsistent, _) => ExpandResult.DoesNotExpand
        | InHole(WrongLength as reason, u) =>
          switch (
            syn_expand_opseq(
              ctx,
              delta,
              opseq |> UHPat.set_err_status_opseq(NotInHole),
            )
          ) {
          | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(dp, _, _, delta) =>
            let gamma = ctx |> Contexts.gamma;
            let delta =
              MetaVarMap.extend_unique(
                delta,
                (u, (PatternHole, ty, gamma)),
              );
            Expands(NonEmptyHole(reason, u, 0, dp), ty, ctx, delta);
          }
        };
      }
    };
  }
  and ana_expand_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHPat.skel,
        seq: UHPat.seq,
        ty: HTyp.t,
      )
      : ExpandResult.t =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      ExpandResult.DoesNotExpand
    | Placeholder(n) =>
      let pn = seq |> Seq.nth_operand(n);
      ana_expand_operand(ctx, delta, pn, ty);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      };
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Hole)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, _ty1, ctx, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Hole)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp2, _ty2, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Expands(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some(ty_elt) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty_elt)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp1, _, ctx, delta) =>
          let ty_list = HTyp.List(ty_elt);
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty_list)) {
          | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(dp2, _, ctx, delta) =>
            let dp = DHPat.Cons(dp1, dp2);
            Expands(dp, ty, ctx, delta);
          };
        }
      }
    }
  and ana_expand_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand, ty: HTyp.t)
      : ExpandResult.t =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(ctx, delta, operand')) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Expands(dp, ty, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => ExpandResult.DoesNotExpand
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = DHPat.EmptyHole(u, 0);
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
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _) => syn_expand_operand(ctx, delta, operand)
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some(ty_elt) => Expands(ListNil, HTyp.List(ty_elt), ctx, delta)
      }
    | Parenthesized(p) => ana_expand(ctx, delta, p, ty)
    | Inj(NotInHole, side, p1) =>
      switch (HTyp.matched_sum(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some((tyL, tyR)) =>
        let ty1 = InjSide.pick(side, tyL, tyR);
        switch (ana_expand(ctx, delta, p1, ty1)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(dp1, ty1, ctx, delta) =>
          let ty =
            switch (side) {
            | L => HTyp.Sum(ty1, tyR)
            | R => HTyp.Sum(tyL, ty1)
            };
          Expands(Inj(side, dp1), ty, ctx, delta);
        };
      }
    };

  let rec renumber_result_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, dp: DHPat.t)
          : (DHPat.t, HoleInstanceInfo.t) =>
    switch (dp) {
    | Wild
    | Var(_)
    | IntLit(_)
    | FloatLit(_)
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
      let (dp1, hii) = renumber_result_only(path, hii, dp1);
      (NonEmptyHole(reason, u, i, dp1), hii);
    | Keyword(u, _, k) =>
      /* TODO: see above */
      let sigma = Environment.empty;
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (Keyword(u, i, k), hii);
    | Inj(side, dp1) =>
      let (dp1, hii) = renumber_result_only(path, hii, dp1);
      (Inj(side, dp1), hii);
    | Pair(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only(path, hii, dp1);
      let (dp2, hii) = renumber_result_only(path, hii, dp2);
      (Pair(dp1, dp2), hii);
    | Cons(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only(path, hii, dp1);
      let (dp2, hii) = renumber_result_only(path, hii, dp2);
      (Cons(dp1, dp2), hii);
    | Ap(dp1, dp2) =>
      let (dp1, hii) = renumber_result_only(path, hii, dp1);
      let (dp2, hii) = renumber_result_only(path, hii, dp2);
      (Pair(dp1, dp2), hii);
    };
};

module Exp = {
  /* closed substitution [d1/x]d2*/
  let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
    switch (d2) {
    | BoundVar(y) =>
      if (Var.eq(x, y)) {
        d1;
      } else {
        d2;
      }
    | FreeVar(_) => d2
    | Keyword(_) => d2
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
    | IntLit(_)
    | FloatLit(_)
    | ListNil(_)
    | Triv => d2
    | Cons(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Cons(d3, d4);
    | BinBoolOp(op, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      BinBoolOp(op, d3, d4);
    | BinIntOp(op, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      BinIntOp(op, d3, d4);
    | BinFloatOp(op, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      BinFloatOp(op, d3, d4);
    | Inj(ty, side, d3) =>
      let d3 = subst_var(d1, x, d3);
      Inj(ty, side, d3);
    | Pair(d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      Pair(d3, d4);
    | ConsistentCase(Case(d3, rules, n)) =>
      let d3 = subst_var(d1, x, d3);
      let rules = subst_var_rules(d1, x, rules);
      ConsistentCase(Case(d3, rules, n));
    | InconsistentBranches(u, i, sigma, Case(d3, rules, n)) =>
      let d3 = subst_var(d1, x, d3);
      let rules = subst_var_rules(d1, x, rules);
      let sigma' = subst_var_env(d1, x, sigma);
      InconsistentBranches(u, i, sigma', Case(d3, rules, n));
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
    | InvalidOperation(d, err) =>
      let d' = subst_var(d1, x, d);
      InvalidOperation(d', err);
    }
  and subst_var_rules =
      (d1: DHExp.t, x: Var.t, rules: list(DHExp.rule)): list(DHExp.rule) =>
    rules
    |> List.map((r: DHExp.rule) =>
         switch (r) {
         | Rule(dp, d2) =>
           if (DHPat.binds_var(x, dp)) {
             r;
           } else {
             Rule(dp, subst_var(d1, x, d2));
           }
         }
       )
  and subst_var_env =
      (d1: DHExp.t, x: Var.t, sigma: Environment.t): Environment.t =>
    sigma
    |> List.map(xd => {
         let (y, d) = xd;
         (y, subst_var(d1, x, d));
       });

  let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
    env
    |> List.fold_left(
         (d2, xd: (Var.t, DHExp.t)) => {
           let (x, d1) = xd;
           subst_var(d1, x, d2);
         },
         d,
       );

  type match_result =
    | Matches(Environment.t)
    | DoesNotMatch
    | Indet;

  let rec matches = (dp: DHPat.t, d: DHExp.t): match_result =>
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
    | (_, InvalidOperation(_)) => Indet
    | (_, FreeVar(_, _, _, _)) => Indet
    | (_, Let(_, _, _)) => Indet
    | (_, FixF(_, _, _)) => DoesNotMatch
    | (_, Lam(_, _, _)) => DoesNotMatch
    | (_, Ap(_, _)) => Indet
    | (_, BinBoolOp(_, _, _)) => Indet
    | (_, BinIntOp(_, _, _)) => Indet
    | (_, BinFloatOp(_, _, _)) => Indet
    | (_, ConsistentCase(Case(_, _, _))) => Indet
    | (BoolLit(b1), BoolLit(b2)) =>
      if (b1 == b2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (BoolLit(_), Cast(d, Bool, Hole)) => matches(dp, d)
    | (BoolLit(_), Cast(d, Hole, Bool)) => matches(dp, d)
    | (BoolLit(_), _) => DoesNotMatch
    | (IntLit(n1), IntLit(n2)) =>
      if (n1 == n2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (IntLit(_), Cast(d, Int, Hole)) => matches(dp, d)
    | (IntLit(_), Cast(d, Hole, Int)) => matches(dp, d)
    | (IntLit(_), _) => DoesNotMatch
    | (FloatLit(n1), FloatLit(n2)) =>
      if (n1 == n2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (FloatLit(_), Cast(d, Float, Hole)) => matches(dp, d)
    | (FloatLit(_), Cast(d, Hole, Float)) => matches(dp, d)
    | (FloatLit(_), _) => DoesNotMatch
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
    | (
        Pair(dp1, dp2),
        Cast(d, Prod([head1, ...tail1]), Prod([head2, ...tail2])),
      ) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d,
        [(head1, head2)],
        List.combine(tail1, tail2),
      )
    | (Pair(_, _), Cast(d, Hole, Prod(_)))
    | (Pair(_, _), Cast(d, Prod(_), Hole)) => matches(dp, d)
    | (Pair(_, _), _) => DoesNotMatch
    | (Triv, Triv) => Matches(Environment.empty)
    | (Triv, Cast(d, Hole, Prod([]))) => matches(dp, d)
    | (Triv, Cast(d, Prod([]), Hole)) => matches(dp, d)
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
        d: DHExp.t,
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
        matches(dp, DHExp.apply_casts(d', side_casts));
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
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Cons(_, _) => DoesNotMatch
    | Pair(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | ConsistentCase(_)
    | InconsistentBranches(_) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    | InvalidOperation(_) => Indet
    }
  and matches_cast_Pair =
      (
        dp1: DHPat.t,
        dp2: DHPat.t,
        d: DHExp.t,
        left_casts: list((HTyp.t, HTyp.t)),
        right_casts: list((HTyp.t, HTyp.t)),
      )
      : match_result =>
    switch (d) {
    | Pair(d1, d2) =>
      switch (matches(dp1, DHExp.apply_casts(d1, left_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env1) =>
        switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
        | DoesNotMatch => DoesNotMatch
        | Indet => Indet
        | Matches(env2) => Matches(Environment.union(env1, env2))
        }
      }
    | Cast(d', Prod([]), Prod([])) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | Cast(d', Prod([head1, ...tail1]), Prod([head2, ...tail2])) =>
      matches_cast_Pair(
        dp1,
        dp2,
        d',
        [(head1, head2), ...left_casts],
        List.combine(tail1, tail2) @ right_casts,
      )
    | Cast(d', Prod(_), Hole)
    | Cast(d', Hole, Prod(_)) =>
      matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
    | Cast(_, _, _) => DoesNotMatch
    | BoundVar(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | Inj(_, _, _) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Cons(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | ConsistentCase(_)
    | InconsistentBranches(_) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    | InvalidOperation(_) => Indet
    }
  and matches_cast_Cons =
      (
        dp1: DHPat.t,
        dp2: DHPat.t,
        d: DHExp.t,
        elt_casts: list((HTyp.t, HTyp.t)),
      )
      : match_result =>
    switch (d) {
    | Cons(d1, d2) =>
      switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
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
        switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
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
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | Inj(_, _, _) => DoesNotMatch
    | ListNil(_) => DoesNotMatch
    | Pair(_, _) => DoesNotMatch
    | Triv => DoesNotMatch
    | ConsistentCase(_)
    | InconsistentBranches(_) => Indet
    | EmptyHole(_, _, _) => Indet
    | NonEmptyHole(_, _, _, _, _) => Indet
    | FailedCast(_, _, _) => Indet
    | InvalidOperation(_) => Indet
    };

  type expand_result_lines =
    | LinesExpand(DHExp.t => DHExp.t, Contexts.t, Delta.t)
    | LinesDoNotExpand;

  module ExpandResult = {
    type t =
      | Expands(DHExp.t, HTyp.t, Delta.t)
      | DoesNotExpand;

    let to_option =
      fun
      | DoesNotExpand => None
      | Expands(pat, ty, delta) => Some((pat, ty, delta));

    let from_option =
      fun
      | None => DoesNotExpand
      | Some((pat, ty, delta)) => Expands(pat, ty, delta);

    let bind = (x: t, ~f: ((DHExp.t, HTyp.t, Delta.t)) => t): t =>
      switch (x) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, ty, delta) => f((dp, ty, delta))
      };
  };

  module Let_syntax = ExpandResult;

  let id_env = (ctx: VarCtx.t): Environment.t =>
    VarMap.map(
      xt => {
        let (x, _) = xt;
        DHExp.BoundVar(x);
      },
      ctx,
    );

  let rec syn_expand =
          (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): ExpandResult.t =>
    syn_expand_block(ctx, delta, e)
  and syn_expand_block =
      (ctx: Contexts.t, delta: Delta.t, block: UHExp.block): ExpandResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ExpandResult.DoesNotExpand
    | Some((leading, conclusion)) =>
      switch (syn_expand_lines(ctx, delta, leading)) {
      | LinesDoNotExpand => ExpandResult.DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (syn_expand_opseq(ctx, delta, conclusion)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and syn_expand_lines =
      (ctx: Contexts.t, delta: Delta.t, lines: list(UHExp.line))
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
      switch (syn_expand_opseq(ctx, delta, e1)) {
      | ExpandResult.DoesNotExpand => LinesDoNotExpand
      | Expands(d1, _, delta) =>
        let prelude = d2 => DHExp.Let(Wild, d1, d2);
        LinesExpand(prelude, ctx, delta);
      }
    | EmptyLine => LinesExpand(d => d, ctx, delta)
    | LetLine(p, ann, def) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let (ctx1, is_recursive_fn) =
          Statics.Exp.ctx_for_let'(ctx, p, ty1, def);
        switch (ana_expand(ctx1, delta, def, ty1)) {
        | ExpandResult.DoesNotExpand => LinesDoNotExpand
        | Expands(d1, ty1', delta) =>
          let d1 =
            switch (is_recursive_fn) {
            | None => d1
            | Some(x) =>
              FixF(
                x,
                ty1',
                subst_var(DHExp.cast(BoundVar(x), ty1', ty1), x, d1),
              )
            };
          let d1 = DHExp.cast(d1, ty1', ty1);
          switch (Pat.ana_expand(ctx, delta, p, ty1)) {
          | Pat.ExpandResult.DoesNotExpand => LinesDoNotExpand
          | Expands(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesExpand(prelude, ctx, delta);
          };
        };
      | None =>
        switch (syn_expand(ctx, delta, def)) {
        | ExpandResult.DoesNotExpand => LinesDoNotExpand
        | Expands(d1, ty1, delta) =>
          switch (Pat.ana_expand(ctx, delta, p, ty1)) {
          | Pat.ExpandResult.DoesNotExpand => LinesDoNotExpand
          | Expands(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesExpand(prelude, ctx, delta);
          }
        }
      }
    }
  and syn_expand_opseq =
      (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHExp.opseq)
      : ExpandResult.t =>
    syn_expand_skel(ctx, delta, skel, seq)
  and syn_expand_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHExp.skel, seq: UHExp.seq)
      : ExpandResult.t =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      syn_expand_operand(ctx, delta, en);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
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
    | BinOp(InHole(WrongLength, _), _, _, _) => ExpandResult.DoesNotExpand
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (Statics.Exp.syn_skel(ctx, skel1, seq)) {
      | None => ExpandResult.DoesNotExpand
      | Some(ty1) =>
        switch (HTyp.matched_arrow(ty1)) {
        | None => ExpandResult.DoesNotExpand
        | Some((ty2, ty)) =>
          let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
          switch (ana_expand_skel(ctx, delta, skel1, seq, ty2_arrow_ty)) {
          | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(d1, ty1', delta) =>
            switch (ana_expand_skel(ctx, delta, skel2, seq, ty2)) {
            | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
            | Expands(d2, ty2', delta) =>
              let dc1 = DHExp.cast(d1, ty1', ty2_arrow_ty);
              let dc2 = DHExp.cast(d2, ty2', ty2);
              let d = DHExp.Ap(dc1, dc2);
              Expands(d, ty, delta);
            }
          };
        }
      }
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHExp.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, delta) = syn_expand_skel(ctx, delta, skel1, seq);
        let%bind (dp2, ty2, delta) = syn_expand_skel(ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, delta), skel) => {
               syn_expand_skel(ctx, delta, skel, seq)
               |> ExpandResult.to_option
               |> Option.map(((dp, ty, delta)) =>
                    ((DHExp.Pair(dp_acc, dp), delta), ty)
                  )
             },
             (DHExp.Pair(dp1, dp2), delta),
           )
        |> Option.map((((dp_acc, delta), tys)) =>
             (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), delta)
           )
        |> ExpandResult.from_option;
      | _ =>
        raise(
          Invalid_argument(
            "Encountered tuple pattern type with less than 2 elements!",
          ),
        )
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_expand_skel(ctx, delta, skel1, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_expand_skel(ctx, delta, skel2, seq, ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let d2c = DHExp.cast(d2, ty2, ty);
          let d = DHExp.Cons(d1, d2c);
          Expands(d, ty, delta);
        };
      }
    | BinOp(NotInHole, (Plus | Minus | Times | Divide) as op, skel1, skel2)
    | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Int)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Int)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Int);
          let dc2 = DHExp.cast(d2, ty2, Int);
          switch (DHExp.BinIntOp.of_op(op)) {
          | None => ExpandResult.DoesNotExpand
          | Some((op, ty)) =>
            let d = DHExp.BinIntOp(op, dc1, dc2);
            Expands(d, ty, delta);
          };
        }
      }
    | BinOp(
        NotInHole,
        (FPlus | FMinus | FTimes | FDivide) as op,
        skel1,
        skel2,
      )
    | BinOp(
        NotInHole,
        (FLessThan | FGreaterThan | FEquals) as op,
        skel1,
        skel2,
      ) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Float)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Float)) {
        | DoesNotExpand => DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Float);
          let dc2 = DHExp.cast(d2, ty2, Float);
          switch (DHExp.BinFloatOp.of_op(op)) {
          | None => DoesNotExpand
          | Some((op, ty)) =>
            let d = DHExp.BinFloatOp(op, dc1, dc2);
            Expands(d, ty, delta);
          };
        }
      }
    | BinOp(NotInHole, (And | Or) as op, skel1, skel2) =>
      switch (ana_expand_skel(ctx, delta, skel1, seq, Bool)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(ctx, delta, skel2, seq, Bool)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Bool);
          let dc2 = DHExp.cast(d2, ty2, Bool);
          switch (DHExp.BinBoolOp.of_op(op)) {
          | None => ExpandResult.DoesNotExpand
          | Some(op) =>
            let d = DHExp.BinBoolOp(op, dc1, dc2);
            Expands(d, Bool, delta);
          };
        }
      }
    }
  and syn_expand_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHExp.operand)
      : ExpandResult.t =>
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(ctx, delta, operand')) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
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
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) => ExpandResult.DoesNotExpand
    | Case(InconsistentBranches(rule_types, u), scrut, rules) =>
      switch (syn_expand(ctx, delta, scrut)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, pat_ty, delta) =>
        let expand_rules =
          List.fold_left2(
            (b, r_t, r) =>
              switch (b) {
              | None => None
              | Some((drs, delta)) =>
                switch (syn_expand_rule(ctx, delta, r, pat_ty, r_t)) {
                | None => None
                | Some((dr, delta)) =>
                  let drs = drs @ [dr];
                  Some((drs, delta));
                }
              },
            Some(([], delta)),
            rule_types,
            rules,
          );
        switch (expand_rules) {
        | None => DoesNotExpand
        | Some((drs, delta)) =>
          let gamma = Contexts.gamma(ctx);
          let sigma = id_env(gamma);
          let delta =
            MetaVarMap.extend_unique(
              delta,
              (u, (ExpressionHole, Hole, gamma)),
            );
          let d = DHExp.Case(d1, drs, 0);
          Expands(InconsistentBranches(u, 0, sigma, d), Hole, delta);
        };
      }
    /* not in hole */
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
      | None => ExpandResult.DoesNotExpand
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
    | IntLit(NotInHole, n) =>
      switch (int_of_string_opt(n)) {
      | Some(n) => Expands(IntLit(n), Int, delta)
      | None => DoesNotExpand
      }
    | FloatLit(NotInHole, f) =>
      switch (TextShape.hazel_float_of_string_opt(f)) {
      | Some(f) => Expands(FloatLit(f), Float, delta)
      | None => DoesNotExpand
      }
    | BoolLit(NotInHole, b) => Expands(BoolLit(b), Bool, delta)
    | ListNil(NotInHole) =>
      let elt_ty = HTyp.Hole;
      Expands(ListNil(elt_ty), List(elt_ty), delta);
    | Parenthesized(body) => syn_expand(ctx, delta, body)
    | Lam(NotInHole, p, ann, body) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Pat.ana_expand(ctx, delta, p, ty1)) {
      | Pat.ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        switch (syn_expand(ctx, delta, body)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d1, ty2, delta) =>
          let d = DHExp.Lam(dp, ty1, d1);
          Expands(d, Arrow(ty1, ty2), delta);
        }
      };
    | Inj(NotInHole, side, body) =>
      switch (syn_expand(ctx, delta, body)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let d = DHExp.Inj(Hole, side, d1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Expands(d, ty, delta);
      }
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      switch (syn_expand(ctx, delta, scrut)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty, delta) =>
        switch (syn_expand_rules(ctx, delta, rules, ty)) {
        | None => DoesNotExpand
        | Some((drs, glb, delta)) =>
          let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
          Expands(d, glb, delta);
        }
      }
    | ApPalette(NotInHole, _name, _serialized_model, _hole_data) => ExpandResult.DoesNotExpand
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
                 let opseq = Seq.ExpOpExp (UHExp.Parenthesized lam) Operators.Exp.Space (UHExp.Parenthesized hexp_ann) in
                 let ap = UHExp.OpSeq (UHExp.associate opseq) opseq in
                 UHExp.Tm NotInHole ap
               )
               expansion in
         ana_expand_exp ctx bound_expansion expansion_ty
       | None -> ExpandResult.DoesNotExpand
       end */
    }
  and syn_expand_rules =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        rules: list(UHExp.rule),
        pat_ty: HTyp.t,
      )
      : option((list(DHExp.rule), HTyp.t, Delta.t)) =>
    switch (Statics.Exp.syn_rules(ctx, rules, pat_ty)) {
    | None => None
    | Some(glb) =>
      let expanded_rule_info =
        List.fold_left(
          (b, r) =>
            switch (b) {
            | None => None
            | Some((drs, delta)) =>
              switch (syn_expand_rule(ctx, delta, r, pat_ty, glb)) {
              | None => None
              | Some((dr, delta)) =>
                let drs = drs @ [dr];
                Some((drs, delta));
              }
            },
          Some(([], delta)),
          rules,
        );
      switch (expanded_rule_info) {
      | None => None
      | Some((drs, delta)) => Some((drs, glb, delta))
      };
    }
  and syn_expand_rule =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        r: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((DHExp.rule, Delta.t)) => {
    let UHExp.Rule(p, clause) = r;
    switch (Pat.ana_expand(ctx, delta, p, pat_ty)) {
    | DoesNotExpand => None
    | Expands(dp, _, ctx, delta) =>
      switch (syn_expand(ctx, delta, clause)) {
      | DoesNotExpand => None
      | Expands(d1, ty1, delta) =>
        Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta))
      }
    };
  }
  and ana_expand =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t, ty: HTyp.t)
      : ExpandResult.t =>
    ana_expand_block(ctx, delta, e, ty)
  and ana_expand_block =
      (ctx: Contexts.t, delta: Delta.t, block: UHExp.block, ty: HTyp.t)
      : ExpandResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ExpandResult.DoesNotExpand
    | Some((leading, conclusion)) =>
      switch (syn_expand_lines(ctx, delta, leading)) {
      | LinesDoNotExpand => ExpandResult.DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (ana_expand_opseq(ctx, delta, conclusion, ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and ana_expand_opseq =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        OpSeq(skel, seq) as opseq: UHExp.opseq,
        ty: HTyp.t,
      )
      : ExpandResult.t => {
    // handle n-tuples
    switch (Statics.Exp.tuple_zip(skel, ty)) {
    | Some(skel_tys) =>
      skel_tys
      |> List.fold_left(
           (
             acc: option((list(DHExp.t), list(HTyp.t), Delta.t)),
             (skel: UHExp.skel, ty: HTyp.t),
           ) =>
             switch (acc) {
             | None => None
             | Some((rev_ds, rev_tys, delta)) =>
               switch (ana_expand_skel(ctx, delta, skel, seq, ty)) {
               | ExpandResult.DoesNotExpand => None
               | Expands(d, ty, delta) =>
                 Some(([d, ...rev_ds], [ty, ...rev_tys], delta))
               }
             },
           Some(([], [], delta)),
         )
      |> (
        fun
        | None => ExpandResult.DoesNotExpand
        | Some((rev_ds, rev_tys, delta)) => {
            let d = rev_ds |> List.rev |> DHExp.mk_tuple;
            let ty =
              switch (rev_tys) {
              | [] => failwith("expected at least 1 element")
              | [ty] => ty
              | _ => HTyp.Prod(rev_tys |> List.rev)
              };
            Expands(d, ty, delta);
          }
      )
    | None =>
      if (List.length(HTyp.get_prod_elements(ty)) == 1) {
        skel
        |> UHExp.get_tuple_elements
        |> List.fold_left(
             (
               acc: option((list(DHExp.t), list(HTyp.t), Delta.t)),
               skel: UHExp.skel,
             ) =>
               switch (acc) {
               | None => None
               | Some((rev_ds, rev_tys, delta)) =>
                 switch (syn_expand_skel(ctx, delta, skel, seq)) {
                 | DoesNotExpand => None
                 | Expands(d, ty, delta) =>
                   Some(([d, ...rev_ds], [ty, ...rev_tys], delta))
                 }
               },
             Some(([], [], delta)),
           )
        |> (
          fun
          | None => ExpandResult.DoesNotExpand
          | Some((rev_ds, rev_tys, delta)) => {
              let d = DHExp.mk_tuple(List.rev(rev_ds));
              let ty =
                switch (rev_tys) {
                | [] => failwith("expected at least 1 element")
                | [ty] => ty
                | _ => HTyp.Prod(rev_tys |> List.rev)
                };
              Expands(d, ty, delta);
            }
        );
      } else {
        switch (opseq |> UHExp.get_err_status_opseq) {
        | NotInHole
        | InHole(TypeInconsistent, _) => ExpandResult.DoesNotExpand
        | InHole(WrongLength as reason, u) =>
          switch (
            syn_expand_opseq(
              ctx,
              delta,
              opseq |> UHExp.set_err_status_opseq(NotInHole),
            )
          ) {
          | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(d, _, delta) =>
            let gamma = ctx |> Contexts.gamma;
            let sigma = gamma |> id_env;
            let delta =
              MetaVarMap.extend_unique(
                delta,
                (u, (ExpressionHole, ty, gamma)),
              );
            Expands(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
          }
        };
      }
    };
  }
  and ana_expand_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHExp.skel,
        seq: UHExp.seq,
        ty: HTyp.t,
      )
      : ExpandResult.t =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      ExpandResult.DoesNotExpand
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      ana_expand_operand(ctx, delta, en, ty);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_expand_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1);
        Expands(d, Hole, delta);
      };
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some(ty_elt) =>
        switch (ana_expand_skel(ctx, delta, skel1, seq, ty_elt)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d1, ty_elt', delta) =>
          let d1c = DHExp.cast(d1, ty_elt', ty_elt);
          let ty_list = HTyp.List(ty_elt);
          switch (ana_expand_skel(ctx, delta, skel2, seq, ty_list)) {
          | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(d2, ty2, delta) =>
            let d2c = DHExp.cast(d2, ty2, ty_list);
            let d = DHExp.Cons(d1c, d2c);
            Expands(d, ty_list, delta);
          };
        }
      }
    | BinOp(
        _,
        Plus | Minus | Times | Divide | FPlus | FMinus | FTimes | FDivide |
        LessThan |
        GreaterThan |
        Equals |
        FLessThan |
        FGreaterThan |
        FEquals |
        And |
        Or |
        Space,
        _,
        _,
      ) =>
      switch (syn_expand_skel(ctx, delta, skel, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d, ty', delta) =>
        if (HTyp.consistent(ty, ty')) {
          Expands(d, ty', delta);
        } else {
          ExpandResult.DoesNotExpand;
        }
      }
    }
  and ana_expand_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHExp.operand, ty: HTyp.t)
      : ExpandResult.t =>
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(ctx, delta, operand')) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        Expands(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      };
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(
        StandardErrStatus(InHole(WrongLength, _)) |
        InconsistentBranches(_, _),
        _,
        _,
      )
    | ApPalette(InHole(WrongLength, _), _, _, _) => ExpandResult.DoesNotExpand
    /* not in hole */
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = DHExp.EmptyHole(u, 0, sigma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Expands(d, ty, delta);
    | Var(NotInHole, InVarHole(reason, u), x) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      let d: DHExp.t =
        switch (reason) {
        | Free => FreeVar(u, 0, sigma, x)
        | Keyword(k) => Keyword(u, 0, sigma, k)
        };
      Expands(d, ty, delta);
    | Parenthesized(body) => ana_expand(ctx, delta, body, ty)
    | Lam(NotInHole, p, ann, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          switch (HTyp.consistent(ty1_ann, ty1_given)) {
          | false => ExpandResult.DoesNotExpand
          | true =>
            switch (Pat.ana_expand(ctx, delta, p, ty1_ann)) {
            | Pat.ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
            | Expands(dp, ty1p, ctx, delta) =>
              switch (ana_expand(ctx, delta, body, ty2)) {
              | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
              | Expands(d1, ty2, delta) =>
                let ty = HTyp.Arrow(ty1p, ty2);
                let d = DHExp.Lam(dp, ty1p, d1);
                Expands(d, ty, delta);
              }
            }
          };
        | None =>
          switch (Pat.ana_expand(ctx, delta, p, ty1_given)) {
          | Pat.ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
          | Expands(dp, ty1, ctx, delta) =>
            switch (ana_expand(ctx, delta, body, ty2)) {
            | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
            | Expands(d1, ty2, delta) =>
              let ty = HTyp.Arrow(ty1, ty2);
              let d = DHExp.Lam(dp, ty1, d1);
              Expands(d, ty, delta);
            }
          }
        }
      }
    | Inj(NotInHole, side, body) =>
      switch (HTyp.matched_sum(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some((ty1, ty2)) =>
        let e1ty = InjSide.pick(side, ty1, ty2);
        switch (ana_expand(ctx, delta, body, e1ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d1, e1ty', delta) =>
          let (ann_ty, ty) =
            switch (side) {
            | L => (ty2, HTyp.Sum(e1ty', ty2))
            | R => (ty1, HTyp.Sum(ty1, e1ty'))
            };
          let d = DHExp.Inj(ann_ty, side, d1);
          Expands(d, ty, delta);
        };
      }
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      switch (syn_expand(ctx, delta, scrut)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(ctx, delta, rules, ty1, ty)) {
        | None => ExpandResult.DoesNotExpand
        | Some((drs, delta)) =>
          let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
          Expands(d, ty, delta);
        }
      }
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => ExpandResult.DoesNotExpand
      | Some(elt_ty) => Expands(ListNil(elt_ty), List(elt_ty), delta)
      }
    | Var(NotInHole, NotInVarHole, _)
    | BoolLit(NotInHole, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | ApPalette(NotInHole, _, _, _) =>
      /* subsumption */
      syn_expand_operand(ctx, delta, operand)
    }
  and ana_expand_rules =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        rules: list(UHExp.rule),
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((list(DHExp.rule), Delta.t)) =>
    rules
    |> List.fold_left(
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
       )
  and ana_expand_rule =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        r: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((DHExp.rule, Delta.t)) => {
    let UHExp.Rule(p, clause) = r;
    switch (Pat.ana_expand(ctx, delta, p, pat_ty)) {
    | Pat.ExpandResult.DoesNotExpand => None
    | Expands(dp, _, ctx, delta) =>
      switch (ana_expand(ctx, delta, clause, clause_ty)) {
      | ExpandResult.DoesNotExpand => None
      | Expands(d1, ty1, delta) =>
        Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta))
      }
    };
  };

  let rec renumber_result_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
          : (DHExp.t, HoleInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
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
    | BinBoolOp(op, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (BinBoolOp(op, d1, d2), hii);
    | BinIntOp(op, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (BinIntOp(op, d1, d2), hii);
    | BinFloatOp(op, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (BinFloatOp(op, d1, d2), hii);
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
    | ConsistentCase(Case(d1, rules, n)) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (drules, hii) = renumber_result_only_rules(path, hii, rules);
      (ConsistentCase(Case(d1, drules, n)), hii);
    | InconsistentBranches(u, _, sigma, Case(d1, rules, n)) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (drules, hii) = renumber_result_only_rules(path, hii, rules);
      (InconsistentBranches(u, i, sigma, Case(d1, drules, n)), hii);
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
    | InvalidOperation(d, err) =>
      let (d, hii) = renumber_result_only(path, hii, d);
      (InvalidOperation(d, err), hii);
    }
  and renumber_result_only_rules =
      (
        path: InstancePath.t,
        hii: HoleInstanceInfo.t,
        rules: list(DHExp.rule),
      )
      : (list(DHExp.rule), HoleInstanceInfo.t) =>
    rules
    |> List.fold_left(
         (b, r: DHExp.rule) => {
           let (rs, hii) = b;
           switch (r) {
           | Rule(dp, d) =>
             let (dp, hii) = Pat.renumber_result_only(path, hii, dp);
             let (d, hii) = renumber_result_only(path, hii, d);
             (rs @ [DHExp.Rule(dp, d)], hii);
           };
         },
         ([], hii),
       );

  let rec renumber_sigmas_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
          : (DHExp.t, HoleInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
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
    | BinBoolOp(op, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (BinBoolOp(op, d1, d2), hii);
    | BinIntOp(op, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (BinIntOp(op, d1, d2), hii);
    | BinFloatOp(op, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (BinFloatOp(op, d1, d2), hii);
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
    | ConsistentCase(Case(d1, rules, n)) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (rules, hii) = renumber_sigmas_only_rules(path, hii, rules);
      (ConsistentCase(Case(d1, rules, n)), hii);
    | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
      let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (rules, hii) = renumber_sigmas_only_rules(path, hii, rules);
      (InconsistentBranches(u, i, sigma, Case(d1, rules, n)), hii);
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
    | InvalidOperation(d, err) =>
      let (d, hii) = renumber_sigmas_only(path, hii, d);
      (InvalidOperation(d, err), hii);
    }
  and renumber_sigmas_only_rules =
      (
        path: InstancePath.t,
        hii: HoleInstanceInfo.t,
        rules: list(DHExp.rule),
      )
      : (list(DHExp.rule), HoleInstanceInfo.t) =>
    rules
    |> List.fold_left(
         (b, r: DHExp.rule) => {
           let (rs, hii) = b;
           switch (r) {
           | Rule(dp, d) =>
             /* pattern holes don't have environments */
             let (d, hii) = renumber_sigmas_only(path, hii, d);
             (rs @ [DHExp.Rule(dp, d)], hii);
           };
         },
         ([], hii),
       )
  and renumber_sigma =
      (
        path: InstancePath.t,
        u: MetaVar.t,
        i: MetaVarInst.t,
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
      : (DHExp.t, HoleInstanceInfo.t) => {
    let (d, hii) = renumber_result_only(path, hii, d);
    renumber_sigmas_only(path, hii, d);
  };
};

module Evaluator = {
  [@deriving sexp]
  type result =
    | InvalidInput(int)
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t);

  /*
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a int literal 2
     4 = boxed value not a int literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
     7 = boxed value not a float literal 1
     8 = boxed value not a float literal 2
   */

  [@deriving sexp]
  type ground_cases =
    | Hole
    | Ground
    | NotGroundOrHole(HTyp.t); /* the argument is the corresponding ground type */

  let grounded_Arrow = NotGroundOrHole(Arrow(Hole, Hole));
  let grounded_Sum = NotGroundOrHole(Sum(Hole, Hole));
  let grounded_Prod = length =>
    NotGroundOrHole(Prod(ListUtil.replicate(length, HTyp.Hole)));
  let grounded_List = NotGroundOrHole(List(Hole));

  let ground_cases_of = (ty: HTyp.t): ground_cases =>
    switch (ty) {
    | Hole => Hole
    | Bool
    | Int
    | Float
    | Arrow(Hole, Hole)
    | Sum(Hole, Hole)
    | List(Hole) => Ground
    | Prod(tys) =>
      if (List.for_all(HTyp.eq(HTyp.Hole), tys)) {
        Ground;
      } else {
        tys |> List.length |> grounded_Prod;
      }
    | Arrow(_, _) => grounded_Arrow
    | Sum(_, _) => grounded_Sum
    | List(_) => grounded_List
    };

  let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
    switch (op) {
    | And => BoolLit(b1 && b2)
    | Or => BoolLit(b1 || b2)
    };

  let eval_bin_int_op = (op: DHExp.BinIntOp.t, n1: int, n2: int): DHExp.t => {
    switch (op) {
    | Minus => IntLit(n1 - n2)
    | Plus => IntLit(n1 + n2)
    | Times => IntLit(n1 * n2)
    | Divide => IntLit(n1 / n2)
    | LessThan => BoolLit(n1 < n2)
    | GreaterThan => BoolLit(n1 > n2)
    | Equals => BoolLit(n1 == n2)
    };
  };

  let eval_bin_float_op =
      (op: DHExp.BinFloatOp.t, f1: float, f2: float): DHExp.t => {
    switch (op) {
    | FPlus => FloatLit(f1 +. f2)
    | FMinus => FloatLit(f1 -. f2)
    | FTimes => FloatLit(f1 *. f2)
    | FDivide => FloatLit(f1 /. f2)
    | FLessThan => BoolLit(f1 < f2)
    | FGreaterThan => BoolLit(f1 > f2)
    | FEquals => BoolLit(f1 == f2)
    };
  };

  let rec evaluate = (d: DHExp.t): result =>
    switch (d) {
    | BoundVar(_) => InvalidInput(1)
    | Let(dp, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (Exp.matches(dp, d1)) {
        | Indet => Indet(d)
        | DoesNotMatch => Indet(d)
        | Matches(env) => evaluate(Exp.subst(env, d2))
        }
      }
    | FixF(x, _, d1) => evaluate(Exp.subst_var(d, x, d1))
    | Lam(_, _, _) => BoxedValue(d)
    | Ap(d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(Lam(dp, _, d3)) =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2)
        | Indet(d2) =>
          switch (Exp.matches(dp, d2)) {
          | DoesNotMatch => Indet(d)
          | Indet => Indet(d)
          | Matches(env) =>
            /* beta rule */
            evaluate(Exp.subst(env, d3))
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
    | IntLit(_)
    | FloatLit(_)
    | Triv => BoxedValue(d)
    | BinBoolOp(op, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(BoolLit(b2)) =>
          BoxedValue(eval_bin_bool_op(op, b1, b2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      }
    | BinIntOp(op, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(IntLit(n1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(IntLit(n2)) =>
          switch (op, n1, n2) {
          | (Divide, _, 0) =>
            Indet(
              InvalidOperation(
                BinIntOp(op, IntLit(n1), IntLit(n2)),
                DivideByZero,
              ),
            )
          | _ => BoxedValue(eval_bin_int_op(op, n1, n2))
          }
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
        }
      }
    | BinFloatOp(op, d1, d2) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(FloatLit(f1) as d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(FloatLit(f2)) =>
          BoxedValue(eval_bin_float_op(op, f1, f2))
        | BoxedValue(_) => InvalidInput(8)
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(7)
      | Indet(d1') =>
        switch (evaluate(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
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
    | ConsistentCase(Case(d1, rules, n)) =>
      evaluate_case(None, d1, rules, n)
    | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
      evaluate_case(Some((u, i, sigma)), d1, rules, n)
    | EmptyHole(_) => Indet(d)
    | NonEmptyHole(reason, u, i, sigma, d1) =>
      switch (evaluate(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
      }
    | FreeVar(_) => Indet(d)
    | Keyword(_) => Indet(d)
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
    | InvalidOperation(d, err) =>
      switch (evaluate(d)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d')
      | Indet(d') => Indet(InvalidOperation(d', err))
      }
    }
  and evaluate_case =
      (
        inconsistent_info,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : result =>
    switch (evaluate(scrut)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(scrut)
    | Indet(scrut) =>
      switch (List.nth_opt(rules, current_rule_index)) {
      | None =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Some(Rule(dp, d)) =>
        switch (Exp.matches(dp, scrut)) {
        | Indet =>
          let case = DHExp.Case(scrut, rules, current_rule_index);
          switch (inconsistent_info) {
          | None => Indet(ConsistentCase(case))
          | Some((u, i, sigma)) =>
            Indet(InconsistentBranches(u, i, sigma, case))
          };
        | Matches(env) => evaluate(Exp.subst(env, d))
        | DoesNotMatch =>
          evaluate_case(
            inconsistent_info,
            scrut,
            rules,
            current_rule_index + 1,
          )
        }
      }
    };
};
