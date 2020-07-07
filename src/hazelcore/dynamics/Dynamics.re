open Sexplib.Std;

/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */

module Pat = {
  module ExpandResult = {
    type t('a) =
      | Expands(DHPat.t, HTyp.t, Contexts.t'('a), Delta.t)
      | DoesNotExpand;

    let to_option =
      fun
      | DoesNotExpand => None
      | Expands(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

    let from_option =
      fun
      | None => DoesNotExpand
      | Some((pat, ty, ctx, delta)) => Expands(pat, ty, ctx, delta);

    let bind =
        (
          x: t('a),
          ~f: ((DHPat.t, HTyp.t, Contexts.t'('a), Delta.t)) => t('a),
        )
        : t('a) =>
      switch (x) {
      | DoesNotExpand => DoesNotExpand
      | Expands(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
      };
  };

  module Let_syntax = ExpandResult;

  let rec syn_expand =
          (ctx: Contexts.t'('a), delta: Delta.t, p: UHPat.t)
          : ExpandResult.t('a) =>
    syn_expand_opseq(ctx, delta, p)
  and syn_expand_opseq =
      (ctx: Contexts.t'('a), delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
      : ExpandResult.t('a) =>
    syn_expand_skel(ctx, delta, skel, seq)
  and syn_expand_skel =
      (
        ctx: Contexts.t'('a),
        delta: Delta.t,
        skel: UHPat.skel,
        seq: UHPat.seq,
      )
      : ExpandResult.t('a) =>
    switch (skel) {
    | Placeholder(n) =>
      syn_expand_operand(ctx, delta, seq |> Seq.nth_operand(n))
    | BinOp(InHole(TypeInconsistent(_) as reason, u), op, skel1, skel2)
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
      (ctx: Contexts.t'('a), delta: Delta.t, operand: UHPat.operand)
      : ExpandResult.t('a) =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent(_) as reason, u))
    | Var(InHole(TypeInconsistent(_) as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent(_) as reason, u), _)
    | FloatLit(InHole(TypeInconsistent(_) as reason, u), _)
    | BoolLit(InHole(TypeInconsistent(_) as reason, u), _)
    | ListNil(InHole(TypeInconsistent(_) as reason, u))
    | Inj(InHole(TypeInconsistent(_) as reason, u), _, _) =>
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
      (ctx: Contexts.t'('a), delta: Delta.t, p: UHPat.t, ty: HTyp.t)
      : ExpandResult.t('a) =>
    ana_expand_opseq(ctx, delta, p, ty)
  and ana_expand_opseq =
      (
        ctx: Contexts.t'('a),
        delta: Delta.t,
        OpSeq(skel, seq) as opseq: UHPat.opseq,
        ty: HTyp.t,
      )
      : ExpandResult.t('a) => {
    // handle n-tuples
    switch (Statics.Pat.tuple_zip(skel, ty)) {
    | Some(skel_tys) =>
      skel_tys
      |> List.fold_left(
           (
             acc: option((list(DHPat.t), Contexts.t'('a), Delta.t)),
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
               acc: option((list(DHPat.t), Contexts.t'('a), Delta.t)),
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
        | InHole(TypeInconsistent(_), _) => ExpandResult.DoesNotExpand
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
        ctx: Contexts.t'('a),
        delta: Delta.t,
        skel: UHPat.skel,
        seq: UHPat.seq,
        ty: HTyp.t,
      )
      : ExpandResult.t('a) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      ExpandResult.DoesNotExpand
    | Placeholder(n) =>
      let pn = seq |> Seq.nth_operand(n);
      ana_expand_operand(ctx, delta, pn, ty);
    | BinOp(InHole(TypeInconsistent(_) as reason, u), op, skel1, skel2) =>
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
      (
        ctx: Contexts.t'('a),
        delta: Delta.t,
        operand: UHPat.operand,
        ty: HTyp.t,
      )
      : ExpandResult.t('a) =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent(_) as reason, u))
    | Var(InHole(TypeInconsistent(_) as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent(_) as reason, u), _)
    | FloatLit(InHole(TypeInconsistent(_) as reason, u), _)
    | BoolLit(InHole(TypeInconsistent(_) as reason, u), _)
    | ListNil(InHole(TypeInconsistent(_) as reason, u))
    | Inj(InHole(TypeInconsistent(_) as reason, u), _, _) =>
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
  let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t => {
    let subst_splices = splice_info =>
      SpliceInfo.update_splice_map(
        splice_info,
        splice_info.splice_map
        |> NatMap.map(((splice_typ, splice_d)) =>
             (splice_typ, splice_d |> OptUtil.map(subst_var(d1, x)))
           ),
      );
    switch (d2) {
    | BoundVar(y) =>
      if (Var.eq(x, y)) {
        d1;
      } else {
        d2;
      }
    | FreeVar(_) => d2
    | FreeLivelit(_, _, _, _) => d2
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
    | LivelitHole(su, si, sigma, lln, splice_info, dargs, model) =>
      let model' =
        if (List.mem(
              x,
              List.map(
                SpliceInfo.var_of_splice_name,
                splice_info.splice_order,
              )
              @ List.map(((v, _, _)) => v, dargs),
            )) {
          model;
        } else {
          subst_var(d1, x, model);
        };
      let splice_info' = subst_splices(splice_info);
      let dargs' =
        dargs
        |> List.map(((v, t, d_opt)) => {
             let new_d = d_opt |> OptUtil.map(subst_var(d1, x));
             (v, t, new_d);
           });
      let sigma' = subst_var_env(d1, x, sigma);
      LivelitHole(su, si, sigma', lln, splice_info', dargs', model');
    | Cast(d, ty1, ty2) =>
      let d' = subst_var(d1, x, d);
      Cast(d', ty1, ty2);
    | FailedCast(d, ty1, ty2) =>
      let d' = subst_var(d1, x, d);
      FailedCast(d', ty1, ty2);
    | InvalidOperation(d, err) =>
      let d' = subst_var(d1, x, d);
      InvalidOperation(d', err);
    };
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

  type names_to_vars_map = NatMap.t(Var.t);
  let to_ctx = (SpliceInfo.{splice_map, _}, param_tys) => {
    let splice_list = NatMap.to_list(splice_map);
    let splice_ctx_list =
      splice_list
      |> List.map(((i, (ty, _))) => {
           let x = SpliceInfo.var_of_splice_name(i);
           (x, ty);
         });
    let var_ctx = VarCtx.of_list(splice_ctx_list @ param_tys);
    let livelit_ctx = LivelitCtx.empty;
    let ctx: Contexts.t'((DHExp.t, HTyp.t)) = (var_ctx, livelit_ctx);

    ctx;
  };

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
    | (_, LivelitHole(_)) => Indet
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
    | FreeLivelit(_, _, _, _) => Indet
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
    | LivelitHole(_) => Indet
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
    | FreeLivelit(_, _, _, _) => Indet
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
    | LivelitHole(_) => Indet
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
    | FreeLivelit(_, _, _, _) => Indet
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
    | LivelitHole(_) => Indet
    | FailedCast(_, _, _) => Indet
    | InvalidOperation(_) => Indet
    };

  type expand_result_lines =
    | LinesExpand(
        DHExp.t => DHExp.t,
        Contexts.t'((DHExp.t, HTyp.t)),
        Delta.t,
      )
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

  let map_livelit_ctx = (f: 'a => 'b, ctx: Contexts.t'('a)): Contexts.t'('b) => {
    let (gamma, livelit_ctx) = ctx;
    let livelit_ctx =
      livelit_ctx
      |> VarMap.map(((_, (defn, closed))) =>
           (defn, closed |> List.map(((s, c)) => (s, f(c))))
         );
    (gamma, livelit_ctx);
  };

  let id_env = (ctx: VarCtx.t): Environment.t =>
    VarMap.map(
      xt => {
        let (x, _) = xt;
        DHExp.BoundVar(x);
      },
      ctx,
    );

  let rec syn_expand =
          (
            ~livelit_holes=false,
            ctx: Contexts.t'((DHExp.t, HTyp.t)),
            delta: Delta.t,
            e: UHExp.t,
          )
          : ExpandResult.t =>
    syn_expand_block(~livelit_holes, ctx, delta, e)
  and syn_expand_block =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        block: UHExp.block,
      )
      : ExpandResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ExpandResult.DoesNotExpand
    | Some((leading, conclusion)) =>
      switch (syn_expand_lines(~livelit_holes, ctx, delta, leading)) {
      | LinesDoNotExpand => ExpandResult.DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (syn_expand_opseq(~livelit_holes, ctx, delta, conclusion)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and syn_expand_lines =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        lines: list(UHExp.line),
      )
      : expand_result_lines =>
    switch (lines) {
    | [] => LinesExpand(d => d, ctx, delta)
    | [line, ...lines] =>
      switch (syn_expand_line(~livelit_holes, ctx, delta, line)) {
      | LinesDoNotExpand => LinesDoNotExpand
      | LinesExpand(prelude_line, ctx, delta) =>
        switch (syn_expand_lines(~livelit_holes, ctx, delta, lines)) {
        | LinesDoNotExpand => LinesDoNotExpand
        | LinesExpand(prelude_lines, ctx, delta) =>
          LinesExpand(d => prelude_line(prelude_lines(d)), ctx, delta)
        }
      }
    }
  and syn_expand_line =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        line: UHExp.line,
      )
      : expand_result_lines =>
    switch (line) {
    | ExpLine(e1) =>
      switch (syn_expand_opseq(~livelit_holes, ctx, delta, e1)) {
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
        switch (ana_expand(~livelit_holes, ctx1, delta, def, ty1)) {
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
        switch (syn_expand(~livelit_holes, ctx, delta, def)) {
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
    | AbbrevLine(lln_new, err_status, lln_old, args) =>
      let ret = (ctx, delta) => LinesExpand(d => d, ctx, delta);
      switch (err_status) {
      | InAbbrevHole(Free, _) => ret(ctx, delta)
      | InAbbrevHole(ExtraneousArgs, _)
      | NotInAbbrevHole =>
        let (gamma, livelit_ctx) = ctx;
        let old_data_opt = LivelitCtx.lookup(livelit_ctx, lln_old);
        switch (old_data_opt) {
        | None => LinesDoNotExpand
        | Some((old_defn, old_closed_args)) =>
          let base_param_tys = old_defn.param_tys;
          let reqd_param_tys =
            base_param_tys |> ListUtil.drop(List.length(old_closed_args));
          let args = args |> ListUtil.take(List.length(reqd_param_tys));
          let adjusted_param_tys =
            reqd_param_tys |> ListUtil.take(List.length(args));
          let delta_expanded_args_opt =
            adjusted_param_tys
            |> List.map(((_, ty)) => ty)
            |> List.combine(args)
            |> ListUtil.map_with_accumulator_opt(
                 (delta, (arg, ty)) =>
                   switch (
                     ana_expand_operand(~livelit_holes, ctx, delta, arg, ty)
                   ) {
                   | ExpandResult.DoesNotExpand => None
                   | Expands(darg, _, delta) => Some((delta, (darg, ty)))
                   },
                 delta,
               );
          switch (delta_expanded_args_opt) {
          | None => LinesDoNotExpand
          | Some((delta, dargs)) =>
            let dargs =
              dargs
              |> List.combine(adjusted_param_tys |> List.map(((s, _)) => s));
            let livelit_ctx =
              LivelitCtx.extend(
                livelit_ctx,
                (lln_new, (old_defn, old_closed_args @ dargs)),
              );
            let ctx = (gamma, livelit_ctx);
            ret(ctx, delta);
          };
        };
      };
    }
  and syn_expand_opseq =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        OpSeq(skel, seq): UHExp.opseq,
      )
      : ExpandResult.t =>
    syn_expand_skel(~livelit_holes, ctx, delta, skel, seq)
  and syn_expand_skel =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        skel: UHExp.skel,
        seq: UHExp.seq,
      )
      : ExpandResult.t =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      syn_expand_operand(~livelit_holes, ctx, delta, en);
    | BinOp(InHole(TypeInconsistent(_) as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (
        syn_expand_skel(~livelit_holes, ctx, delta, skel_not_in_hole, seq)
      ) {
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
      let livelit_ap_check =
        LivelitUtil.check_livelit(
          ~permit_insufficient_params_hole=true,
          ~permit_insufficient_args=true,
          ctx,
          seq,
          skel,
        );
      switch (livelit_ap_check) {
      | Some((
          ApLivelitData(llu, _, lln, model, splice_info),
          livelit_defn,
          closed_dargs,
          reqd_param_tys,
          args,
        )) =>
        let args_opt =
          args
          |> ListUtil.map_with_accumulator_opt(
               ((), arg) =>
                 switch (arg) {
                 | Skel.Placeholder(n) =>
                   Some(((), seq |> Seq.nth_operand(n) |> UHExp.Block.wrap))
                 | BinOp(_) =>
                   // This heavily assumes that Space will always retain highest precedence
                   None
                 },
               (),
             );
        switch (args_opt) {
        | None => DoesNotExpand
        | Some((_, args)) =>
          syn_expand_ApLivelit(
            ~livelit_holes,
            ctx,
            delta,
            llu,
            lln,
            model,
            splice_info,
            livelit_defn,
            closed_dargs,
            reqd_param_tys,
            args,
          )
        };
      | _ =>
        let ctx' = ctx |> map_livelit_ctx(((_, ty)) => ty);
        switch (Statics.Exp.syn_skel(ctx', skel1, seq)) {
        | None => ExpandResult.DoesNotExpand
        | Some(ty1) =>
          switch (HTyp.matched_arrow(ty1)) {
          | None => ExpandResult.DoesNotExpand
          | Some((ty2, ty)) =>
            let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
            switch (
              ana_expand_skel(
                ~livelit_holes,
                ctx,
                delta,
                skel1,
                seq,
                ty2_arrow_ty,
              )
            ) {
            | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
            | Expands(d1, ty1', delta) =>
              switch (
                ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, ty2)
              ) {
              | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
              | Expands(d2, ty2', delta) =>
                let dc1 = DHExp.cast(d1, ty1', ty2_arrow_ty);
                let dc2 = DHExp.cast(d2, ty2', ty2);
                let d = DHExp.Ap(dc1, dc2);
                Expands(d, ty, delta);
              }
            };
          }
        };
      };
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHExp.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, delta) =
          syn_expand_skel(~livelit_holes, ctx, delta, skel1, seq);
        let%bind (dp2, ty2, delta) =
          syn_expand_skel(~livelit_holes, ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, delta), skel) => {
               syn_expand_skel(~livelit_holes, ctx, delta, skel, seq)
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
      switch (syn_expand_skel(~livelit_holes, ctx, delta, skel1, seq)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d2, ty2, delta) =>
          let d2c = DHExp.cast(d2, ty2, ty);
          let d = DHExp.Cons(d1, d2c);
          Expands(d, ty, delta);
        };
      }
    | BinOp(NotInHole, (Plus | Minus | Times | Divide) as op, skel1, skel2)
    | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
      switch (ana_expand_skel(~livelit_holes, ctx, delta, skel1, seq, Int)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, Int)) {
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
      switch (ana_expand_skel(~livelit_holes, ctx, delta, skel1, seq, Float)) {
      | DoesNotExpand => DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (
          ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, Float)
        ) {
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
      switch (ana_expand_skel(~livelit_holes, ctx, delta, skel1, seq, Bool)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, Bool)) {
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
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        operand: UHExp.operand,
      )
      : ExpandResult.t =>
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent(_) as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent(_) as reason, u), _)
    | FloatLit(InHole(TypeInconsistent(_) as reason, u), _)
    | BoolLit(InHole(TypeInconsistent(_) as reason, u), _)
    | ListNil(InHole(TypeInconsistent(_) as reason, u))
    | Lam(InHole(TypeInconsistent(_) as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent(_) as reason, u), _, _)
    | Case(
        StandardErrStatus(InHole(TypeInconsistent(_) as reason, u)),
        _,
        _,
      )
    | ApLivelit(_, InHole(TypeInconsistent(None) as reason, u), _, _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(~livelit_holes, ctx, delta, operand')) {
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
    | ApLivelit(_, InHole(WrongLength, _), _, _, _, _) => DoesNotExpand
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
    | FreeLivelit(u, name) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, Hole, gamma)));
      let d = DHExp.FreeLivelit(u, 0, sigma, name);
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
    | Parenthesized(body) => syn_expand(~livelit_holes, ctx, delta, body)
    | Lam(NotInHole, p, ann, body) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Pat.ana_expand(ctx, delta, p, ty1)) {
      | Pat.ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(dp, _, ctx, delta) =>
        switch (syn_expand(~livelit_holes, ctx, delta, body)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d1, ty2, delta) =>
          let d = DHExp.Lam(dp, ty1, d1);
          Expands(d, Arrow(ty1, ty2), delta);
        }
      };
    | Inj(NotInHole, side, body) =>
      switch (syn_expand(~livelit_holes, ctx, delta, body)) {
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
    | ApLivelit(
        llu,
        (NotInHole | InHole(TypeInconsistent(Some(InsufficientParams)), _)) as err_status,
        _,
        name,
        serialized_model,
        si,
      ) =>
      let livelit_ctx = Contexts.livelit_ctx(ctx);
      switch (LivelitCtx.lookup(livelit_ctx, name)) {
      | None => DoesNotExpand
      | Some((livelit_defn, closed_dargs)) =>
        let reqd_param_tys =
          livelit_defn.param_tys |> ListUtil.drop(List.length(closed_dargs));
        switch (err_status, reqd_param_tys) {
        | (NotInHole, [_, ..._])
        | (InHole(TypeInconsistent(Some(InsufficientParams)), _), []) =>
          DoesNotExpand
        | _ =>
          syn_expand_ApLivelit(
            ~livelit_holes,
            ctx,
            delta,
            llu,
            name,
            serialized_model,
            si,
            livelit_defn,
            closed_dargs,
            reqd_param_tys,
            [],
          )
        };
      };
    }
  and syn_expand_ApLivelit =
      (
        ~livelit_holes,
        ctx,
        delta,
        llu,
        lln,
        serialized_model,
        si,
        livelit_defn,
        closed_dargs,
        reqd_param_tys,
        args,
      ) => {
    let all_param_tys = livelit_defn.param_tys;
    let closed_dargs =
      all_param_tys
      |> ListUtil.take(closed_dargs |> List.length)
      |> List.combine(closed_dargs)
      |> List.map((((s, (darg, _)), (_, ty))) => (s, ty, darg));
    let expansion_ty = livelit_defn.expansion_ty;
    let expand = livelit_defn.expand;
    let proto_expansion = expand(serialized_model);
    let proto_elaboration_ctx = to_ctx(si, all_param_tys);
    let proto_elaboration_result =
      ana_expand(
        ~livelit_holes,
        proto_elaboration_ctx,
        delta,
        proto_expansion,
        expansion_ty,
      );
    switch (proto_elaboration_result) {
    | DoesNotExpand => DoesNotExpand
    | Expands(proto_elaboration, _, delta) =>
      // Like List.combine, but pads missing args with None
      let rec params_args = (p, a) =>
        switch (p, a) {
        | ([], _) => []
        | ([ph, ...pt], []) => [(ph, None), ...params_args(pt, [])]
        | ([ph, ...pt], [ah, ...at]) => [
            (ph, Some(ah)),
            ...params_args(pt, at),
          ]
        };
      // expand the args
      let dargs_opt =
        params_args(reqd_param_tys, args)
        |> ListUtil.map_with_accumulator_opt(
             (delta, ((name, ty), arg_opt)) =>
               switch (arg_opt) {
               | None => Some((delta, (name, ty, None)))
               | Some(arg) =>
                 switch (ana_expand(~livelit_holes, ctx, delta, arg, ty)) {
                 | DoesNotExpand => None
                 | Expands(d, _, delta) =>
                   Some((delta, (name, ty, Some(d))))
                 }
               },
             delta,
           );
      switch (dargs_opt) {
      | None => DoesNotExpand
      | Some((delta, dargs)) =>
        // expand the splices
        let si_opt =
          si.splice_map
          |> ListUtil.map_with_accumulator_opt(
               (delta, (name, (ty, exp))) =>
                 switch (ana_expand(~livelit_holes, ctx, delta, exp, ty)) {
                 | DoesNotExpand => None
                 | Expands(d, _, delta) => Some((delta, (name, (ty, d))))
                 },
               delta,
             );
        switch (si_opt) {
        | None => DoesNotExpand
        | Some((delta, sim)) =>
          let si =
            SpliceInfo.update_splice_map(
              si,
              sim |> List.map(((name, (ty, d))) => (name, (ty, Some(d)))),
            );
          let dargs_opt' =
            dargs
            |> ListUtil.map_with_accumulator_opt(
                 ((), (v, t, arg_opt)) =>
                   arg_opt |> OptUtil.map(arg => ((), (v, t, arg))),
                 (),
               );
          let rslt =
            switch (dargs_opt') {
            | Some(((), dargs')) when !livelit_holes =>
              // subst each splice and arg into the elab
              wrap_proto_expansion(
                proto_elaboration,
                sim,
                closed_dargs @ dargs',
              )
            | _ =>
              let gamma = Contexts.gamma(ctx);
              let sigma = id_env(gamma);
              let closed_dargs =
                closed_dargs
                |> List.map(((s, ty, darg)) => (s, ty, Some(darg)));
              DHExp.LivelitHole(
                llu,
                0,
                sigma,
                lln,
                si,
                closed_dargs @ dargs,
                proto_elaboration,
              );
            };
          Expands(rslt, expansion_ty, delta);
        };
      };
    };
  }
  and syn_expand_rules =
      (
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        rules: list(UHExp.rule),
        pat_ty: HTyp.t,
      )
      : option((list(DHExp.rule), HTyp.t, Delta.t)) => {
    let ctx' = ctx |> map_livelit_ctx(((_, ty)) => ty);
    switch (Statics.Exp.syn_rules(ctx', rules, pat_ty)) {
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
    };
  }
  and syn_expand_rule =
      (
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
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
  and wrap_proto_expansion =
      (
        proto_expansion,
        sim: SpliceInfo.splice_map(DHExp.t),
        dargs: list((Var.t, HTyp.t, DHExp.t)),
      )
      : DHExp.t => {
    let wrap_ap = (var, ty, body, arg) =>
      DHExp.Ap(DHExp.Lam(DHPat.Var(var), ty, body), arg);
    let post_args_expansion =
      dargs
      |> List.fold_left(
           (expansion, (name, ty, arg)) =>
             wrap_ap(name, ty, expansion, arg),
           proto_expansion,
         );
    sim
    |> List.fold_left(
         (expansion, (name, (ty, d))) =>
           wrap_ap(SpliceInfo.var_of_splice_name(name), ty, expansion, d),
         post_args_expansion,
       );
  }
  and ana_expand =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        e: UHExp.t,
        ty: HTyp.t,
      )
      : ExpandResult.t =>
    ana_expand_block(~livelit_holes, ctx, delta, e, ty)
  and ana_expand_block =
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        block: UHExp.block,
        ty: HTyp.t,
      )
      : ExpandResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ExpandResult.DoesNotExpand
    | Some((leading, conclusion)) =>
      switch (syn_expand_lines(~livelit_holes, ctx, delta, leading)) {
      | LinesDoNotExpand => ExpandResult.DoesNotExpand
      | LinesExpand(prelude, ctx, delta) =>
        switch (ana_expand_opseq(~livelit_holes, ctx, delta, conclusion, ty)) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d, ty, delta) => Expands(prelude(d), ty, delta)
        }
      }
    }
  and ana_expand_opseq =
      (
        ~livelit_holes: bool,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
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
               switch (
                 ana_expand_skel(~livelit_holes, ctx, delta, skel, seq, ty)
               ) {
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
                 switch (
                   syn_expand_skel(~livelit_holes, ctx, delta, skel, seq)
                 ) {
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
        | InHole(TypeInconsistent(_), _) => ExpandResult.DoesNotExpand
        | InHole(WrongLength as reason, u) =>
          switch (
            syn_expand_opseq(
              ~livelit_holes,
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
        ~livelit_holes: bool,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
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
      ana_expand_operand(~livelit_holes, ctx, delta, en, ty);
    | BinOp(InHole(TypeInconsistent(_) as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (
        syn_expand_skel(~livelit_holes, ctx, delta, skel_not_in_hole, seq)
      ) {
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
        switch (
          ana_expand_skel(~livelit_holes, ctx, delta, skel1, seq, ty_elt)
        ) {
        | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
        | Expands(d1, ty_elt', delta) =>
          let d1c = DHExp.cast(d1, ty_elt', ty_elt);
          let ty_list = HTyp.List(ty_elt);
          switch (
            ana_expand_skel(~livelit_holes, ctx, delta, skel2, seq, ty_list)
          ) {
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
      switch (syn_expand_skel(~livelit_holes, ctx, delta, skel, seq)) {
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
      (
        ~livelit_holes,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
        delta: Delta.t,
        operand: UHExp.operand,
        ty: HTyp.t,
      )
      : ExpandResult.t =>
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent(_) as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent(_) as reason, u), _)
    | FloatLit(InHole(TypeInconsistent(_) as reason, u), _)
    | BoolLit(InHole(TypeInconsistent(_) as reason, u), _)
    | ListNil(InHole(TypeInconsistent(_) as reason, u))
    | Lam(InHole(TypeInconsistent(_) as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent(_) as reason, u), _, _)
    | Case(
        StandardErrStatus(InHole(TypeInconsistent(_) as reason, u)),
        _,
        _,
      )
    | ApLivelit(_, InHole(TypeInconsistent(None) as reason, u), _, _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_expand_operand(~livelit_holes, ctx, delta, operand')) {
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
    | ApLivelit(_, InHole(WrongLength, _), _, _, _, _) => DoesNotExpand
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
    | FreeLivelit(u, name) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      let d = DHExp.FreeLivelit(u, 0, sigma, name);
      Expands(d, ty, delta);
    | Parenthesized(body) => ana_expand(~livelit_holes, ctx, delta, body, ty)
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
              switch (ana_expand(~livelit_holes, ctx, delta, body, ty2)) {
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
            switch (ana_expand(~livelit_holes, ctx, delta, body, ty2)) {
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
        switch (ana_expand(~livelit_holes, ctx, delta, body, e1ty)) {
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
      switch (syn_expand(~livelit_holes, ctx, delta, scrut)) {
      | ExpandResult.DoesNotExpand => ExpandResult.DoesNotExpand
      | Expands(d1, ty1, delta) =>
        switch (ana_expand_rules(~livelit_holes, ctx, delta, rules, ty1, ty)) {
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
    | ApLivelit(
        _,
        NotInHole | InHole(TypeInconsistent(Some(InsufficientParams)), _),
        _,
        _,
        _,
        _,
      ) =>
      /* subsumption */
      syn_expand_operand(~livelit_holes, ctx, delta, operand)
    }
  and ana_expand_rules =
      (
        ~livelit_holes: bool,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
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
             switch (
               ana_expand_rule(
                 ~livelit_holes,
                 ctx,
                 delta,
                 r,
                 pat_ty,
                 clause_ty,
               )
             ) {
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
        ~livelit_holes: bool,
        ctx: Contexts.t'((DHExp.t, HTyp.t)),
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
      switch (ana_expand(~livelit_holes, ctx, delta, clause, clause_ty)) {
      | ExpandResult.DoesNotExpand => None
      | Expands(d1, ty1, delta) =>
        Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta))
      }
    };
  };

  let _renumber_splices =
      (renumberer, splice_info: SpliceInfo.t(option(DHExp.t)), hii, llii) => {
    let (sim, hii, llii) =
      (NatMap.empty, hii, llii)
      |> NatMap.fold(
           splice_info.splice_map,
           ((sim, hii, llii), (name, (typ, dexp_opt))) =>
           switch (dexp_opt) {
           | None => (sim, hii, llii)
           | Some(dexp) =>
             let (dexp, hii, llii) = renumberer(hii, llii, dexp);
             let sim =
               NatMap.insert_or_update(sim, (name, (typ, Some(dexp))));
             (sim, hii, llii);
           }
         );
    let splice_info = SpliceInfo.update_splice_map(splice_info, sim);
    (splice_info, hii, llii);
  };

  let _renumber_dargs =
      (
        renumberer,
        dargs: list((Var.t, HTyp.t, option(DHExp.t))),
        hii,
        llii,
      ) => {
    let ((hii, llii), dargs) =
      dargs
      |> ListUtil.map_with_accumulator(
           ((hii, llii), (v, t, darg_opt)) =>
             switch (darg_opt) {
             | None => ((hii, llii), (v, t, None))
             | Some(darg) =>
               let (darg, hii, llii) = renumberer(hii, llii, darg);
               ((hii, llii), (v, t, Some(darg)));
             },
           (hii, llii),
         );
    (dargs, hii, llii);
  };

  let rec renumber_result_only =
          (
            path: InstancePath.t,
            hii: HoleInstanceInfo.t,
            llii: LivelitInstanceInfo.t,
            d: DHExp.t,
          )
          : (DHExp.t, HoleInstanceInfo.t, LivelitInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | ListNil(_)
    | Triv => (d, hii, llii)
    | Let(dp, d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (Let(dp, d1, d2), hii, llii);
    | FixF(x, ty, d1) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (FixF(x, ty, d1), hii, llii);
    | Lam(x, ty, d1) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (Lam(x, ty, d1), hii, llii);
    | Ap(d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (Ap(d1, d2), hii, llii);
    | BinBoolOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (BinBoolOp(op, d1, d2), hii, llii);
    | BinIntOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (BinIntOp(op, d1, d2), hii, llii);
    | BinFloatOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (BinFloatOp(op, d1, d2), hii, llii);
    | Inj(ty, side, d1) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (Inj(ty, side, d1), hii, llii);
    | Pair(d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (Pair(d1, d2), hii, llii);
    | Cons(d1, d2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_result_only(path, hii, llii, d2);
      (Cons(d1, d2), hii, llii);
    | ConsistentCase(Case(d1, rules, n)) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (drules, hii, llii) =
        renumber_result_only_rules(path, hii, llii, rules);
      (ConsistentCase(Case(d1, drules, n)), hii, llii);
    | InconsistentBranches(u, _, sigma, Case(d1, rules, n)) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      let (drules, hii, llii) =
        renumber_result_only_rules(path, hii, llii, rules);
      (InconsistentBranches(u, i, sigma, Case(d1, drules, n)), hii, llii);
    | EmptyHole(u, _, sigma) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (EmptyHole(u, i, sigma), hii, llii);
    | NonEmptyHole(reason, u, _, sigma, d1) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (NonEmptyHole(reason, u, i, sigma, d1), hii, llii);
    | FreeVar(u, _, sigma, x) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (FreeVar(u, i, sigma, x), hii, llii);
    | Keyword(u, _, sigma, k) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (Keyword(u, i, sigma, k), hii, llii);
    | FreeLivelit(u, _, sigma, name) =>
      let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
      (FreeLivelit(u, i, sigma, name), hii, llii);
    | LivelitHole(su, _, sigma, lln, splice_info, dargs, model) =>
      let (splice_info, hii, llii) =
        _renumber_splices(
          renumber_result_only(path),
          splice_info,
          hii,
          llii,
        );
      let (dargs, hii, llii) =
        _renumber_dargs(renumber_result_only(path), dargs, hii, llii);
      let (si, llii) =
        LivelitInstanceInfo.next(
          llii,
          su,
          sigma,
          path,
          (splice_info, dargs |> List.map(((v, _, darg)) => (v, darg))),
        );
      (
        LivelitHole(su, si, sigma, lln, splice_info, dargs, model),
        hii,
        llii,
      );
    | Cast(d1, ty1, ty2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (Cast(d1, ty1, ty2), hii, llii);
    | FailedCast(d1, ty1, ty2) =>
      let (d1, hii, llii) = renumber_result_only(path, hii, llii, d1);
      (FailedCast(d1, ty1, ty2), hii, llii);
    | InvalidOperation(d, err) =>
      let (d, hii, llii) = renumber_result_only(path, hii, llii, d);
      (InvalidOperation(d, err), hii, llii);
    }
  and renumber_result_only_rules =
      (
        path: InstancePath.t,
        hii: HoleInstanceInfo.t,
        llii: LivelitInstanceInfo.t,
        rules: list(DHExp.rule),
      )
      : (list(DHExp.rule), HoleInstanceInfo.t, LivelitInstanceInfo.t) =>
    rules
    |> List.fold_left(
         (b, r: DHExp.rule) => {
           let (rs, hii, llii) = b;
           switch (r) {
           | Rule(dp, d) =>
             let (dp, hii) = Pat.renumber_result_only(path, hii, dp);
             let (d, hii, llii) = renumber_result_only(path, hii, llii, d);
             (rs @ [DHExp.Rule(dp, d)], hii, llii);
           };
         },
         ([], hii, llii),
       );

  let rec renumber_sigmas_only =
          (
            path: InstancePath.t,
            hii: HoleInstanceInfo.t,
            llii: LivelitInstanceInfo.t,
            d: DHExp.t,
          )
          : (DHExp.t, HoleInstanceInfo.t, LivelitInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | ListNil(_)
    | Triv => (d, hii, llii)
    | Let(dp, d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (Let(dp, d1, d2), hii, llii);
    | FixF(x, ty, d1) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (FixF(x, ty, d1), hii, llii);
    | Lam(x, ty, d1) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (Lam(x, ty, d1), hii, llii);
    | Ap(d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (Ap(d1, d2), hii, llii);
    | BinBoolOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (BinBoolOp(op, d1, d2), hii, llii);
    | BinIntOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (BinIntOp(op, d1, d2), hii, llii);
    | BinFloatOp(op, d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (BinFloatOp(op, d1, d2), hii, llii);
    | Inj(ty, side, d1) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (Inj(ty, side, d1), hii, llii);
    | Pair(d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (Pair(d1, d2), hii, llii);
    | Cons(d1, d2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (d2, hii, llii) = renumber_sigmas_only(path, hii, llii, d2);
      (Cons(d1, d2), hii, llii);
    | ConsistentCase(Case(d1, rules, n)) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (rules, hii, llii) =
        renumber_sigmas_only_rules(path, hii, llii, rules);
      (ConsistentCase(Case(d1, rules, n)), hii, llii);
    | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      let (rules, hii, llii) =
        renumber_sigmas_only_rules(path, hii, llii, rules);
      (InconsistentBranches(u, i, sigma, Case(d1, rules, n)), hii, llii);
    | EmptyHole(u, i, sigma) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (EmptyHole(u, i, sigma), hii, llii);
    | NonEmptyHole(reason, u, i, sigma, d1) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (NonEmptyHole(reason, u, i, sigma, d1), hii, llii);
    | FreeVar(u, i, sigma, x) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (FreeVar(u, i, sigma, x), hii, llii);
    | Keyword(u, i, sigma, k) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (Keyword(u, i, sigma, k), hii, llii);
    | FreeLivelit(u, i, sigma, name) =>
      let (sigma, hii, llii) = renumber_sigma(path, u, i, hii, llii, sigma);
      let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
      (FreeLivelit(u, i, sigma, name), hii, llii);
    | LivelitHole(su, si, sigma, lln, splice_info, dargs, model) =>
      let (sigma, hii, llii) =
        renumber_sigma(path, su, si, hii, llii, sigma);
      let (splice_info, hii, llii) =
        _renumber_splices(
          renumber_sigmas_only(path),
          splice_info,
          hii,
          llii,
        );
      let (dargs, hii, llii) =
        _renumber_dargs(renumber_sigmas_only(path), dargs, hii, llii);
      let llii =
        LivelitInstanceInfo.update_environment(
          llii,
          (su, si),
          sigma,
          (splice_info, dargs |> List.map(((v, _, darg)) => (v, darg))),
        );
      (
        LivelitHole(su, si, sigma, lln, splice_info, dargs, model),
        hii,
        llii,
      );
    | Cast(d1, ty1, ty2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (Cast(d1, ty1, ty2), hii, llii);
    | FailedCast(d1, ty1, ty2) =>
      let (d1, hii, llii) = renumber_sigmas_only(path, hii, llii, d1);
      (FailedCast(d1, ty1, ty2), hii, llii);
    | InvalidOperation(d, err) =>
      let (d, hii, llii) = renumber_sigmas_only(path, hii, llii, d);
      (InvalidOperation(d, err), hii, llii);
    }
  and renumber_sigmas_only_rules =
      (
        path: InstancePath.t,
        hii: HoleInstanceInfo.t,
        llii: LivelitInstanceInfo.t,
        rules: list(DHExp.rule),
      )
      : (list(DHExp.rule), HoleInstanceInfo.t, LivelitInstanceInfo.t) =>
    rules
    |> List.fold_left(
         (b, r: DHExp.rule) => {
           let (rs, hii, llii) = b;
           switch (r) {
           | Rule(dp, d) =>
             /* pattern holes don't have environments */
             let (d, hii, llii) = renumber_sigmas_only(path, hii, llii, d);
             (rs @ [DHExp.Rule(dp, d)], hii, llii);
           };
         },
         ([], hii, llii),
       )
  and renumber_sigma =
      (
        path: InstancePath.t,
        u: MetaVar.t,
        i: MetaVarInst.t,
        hii: HoleInstanceInfo.t,
        llii: LivelitInstanceInfo.t,
        sigma: Environment.t,
      )
      : (Environment.t, HoleInstanceInfo.t, LivelitInstanceInfo.t) => {
    let (sigma, hii, llii) =
      List.fold_right(
        (
          xd: (Var.t, DHExp.t),
          acc: (Environment.t, HoleInstanceInfo.t, LivelitInstanceInfo.t),
        ) => {
          let (x, d) = xd;
          let (sigma_in, hii, llii) = acc;
          let path = [((u, i), x), ...path];
          let (d, hii, llii) = renumber_result_only(path, hii, llii, d);
          let sigma_out = [(x, d), ...sigma_in];
          (sigma_out, hii, llii);
        },
        sigma,
        ([], hii, llii),
      );

    List.fold_right(
      (
        xd: (Var.t, DHExp.t),
        acc: (Environment.t, HoleInstanceInfo.t, LivelitInstanceInfo.t),
      ) => {
        let (x, d) = xd;
        let (sigma_in, hii, llii) = acc;
        let path = [((u, i), x), ...path];
        let (d, hii, llii) = renumber_sigmas_only(path, hii, llii, d);
        let sigma_out = [(x, d), ...sigma_in];
        (sigma_out, hii, llii);
      },
      sigma,
      ([], hii, llii),
    );
  };

  let renumber =
      (
        path: InstancePath.t,
        hii: HoleInstanceInfo.t,
        llii: LivelitInstanceInfo.t,
        d: DHExp.t,
      )
      : (DHExp.t, HoleInstanceInfo.t, LivelitInstanceInfo.t) => {
    let (d, hii, llii) = renumber_result_only(path, hii, llii, d);
    renumber_sigmas_only(path, hii, llii, d);
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
     9 = attempt to evaluate LivelitHole with missing splices
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

  let rec evaluate = (~eval_livelit_holes=false, d: DHExp.t): result => {
    let _eval = evaluate(~eval_livelit_holes);
    switch (d) {
    | BoundVar(_) => InvalidInput(1)
    | Let(dp, d1, d2) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1)
      | Indet(d1) =>
        switch (Exp.matches(dp, d1)) {
        | Indet => Indet(d)
        | DoesNotMatch => Indet(d)
        | Matches(env) => _eval(Exp.subst(env, d2))
        }
      }
    | FixF(x, _, d1) => _eval(Exp.subst_var(d, x, d1))
    | Lam(_, _, _) => BoxedValue(d)
    | Ap(d1, d2) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(Lam(dp, _, d3)) =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2)
        | Indet(d2) =>
          switch (Exp.matches(dp, d2)) {
          | DoesNotMatch => Indet(d)
          | Indet => Indet(d)
          | Matches(env) =>
            /* beta rule */
            _eval(Exp.subst(env, d3))
          }
        }
      | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
      | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') =>
          /* ap cast rule */
          _eval(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
        }
      | BoxedValue(_) => InvalidInput(2)
      | Indet(d1') =>
        switch (_eval(d2)) {
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
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(BoolLit(b1) as d1') =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(BoolLit(b2)) =>
          BoxedValue(eval_bin_bool_op(op, b1, b2))
        | BoxedValue(_) => InvalidInput(3)
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(4)
      | Indet(d1') =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      }
    | BinIntOp(op, d1, d2) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(IntLit(n1) as d1') =>
        switch (_eval(d2)) {
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
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
        }
      }
    | BinFloatOp(op, d1, d2) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(FloatLit(f1) as d1') =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(FloatLit(f2)) =>
          BoxedValue(eval_bin_float_op(op, f1, f2))
        | BoxedValue(_) => InvalidInput(8)
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
        }
      | BoxedValue(_) => InvalidInput(7)
      | Indet(d1') =>
        switch (_eval(d2)) {
        | InvalidInput(msg) => InvalidInput(msg)
        | BoxedValue(d2')
        | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
        }
      }
    | Inj(ty, side, d1) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1'))
      | Indet(d1') => Indet(Inj(ty, side, d1'))
      }
    | Pair(d1, d2) =>
      switch (_eval(d1), _eval(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
      }
    | Cons(d1, d2) =>
      switch (_eval(d1), _eval(d2)) {
      | (InvalidInput(msg), _)
      | (_, InvalidInput(msg)) => InvalidInput(msg)
      | (Indet(d1), Indet(d2))
      | (Indet(d1), BoxedValue(d2))
      | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2))
      | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Cons(d1, d2))
      }
    | ConsistentCase(Case(d1, rules, n)) =>
      evaluate_case(~eval_livelit_holes, None, d1, rules, n)
    | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
      evaluate_case(~eval_livelit_holes, Some((u, i, sigma)), d1, rules, n)
    | EmptyHole(_) => Indet(d)
    | NonEmptyHole(reason, u, i, sigma, d1) =>
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
      }
    | FreeVar(_) => Indet(d)
    | Keyword(_) => Indet(d)
    | FreeLivelit(_, _, _, _) => Indet(d)
    | LivelitHole(su, si, sigma, lln, splice_info, dargs, model) =>
      // evaluate the splices
      let sim_res =
        Result.Ok(NatMap.empty)
        |> NatMap.fold(
             splice_info.splice_map, (sim_res, (name, (typ, dexp_opt))) =>
             Result.bind(sim_res, sim => {
               switch (dexp_opt) {
               | None => Result.Ok(sim)
               | Some(dexp) =>
                 switch (_eval(dexp)) {
                 | InvalidInput(msg) => Result.Error(msg)
                 | BoxedValue(evald)
                 | Indet(evald) =>
                   Result.Ok(
                     NatMap.insert_or_update(
                       sim,
                       (name, (typ, Some(evald))),
                     ),
                   )
                 }
               }
             })
           );
      // evaluate dargs
      let dargs_res =
        List.fold_right(
          ((v, t, darg_opt), res) =>
            Result.bind(res, acc =>
              switch (darg_opt) {
              | None => Result.Ok([(v, t, None), ...acc])
              | Some(darg) =>
                switch (_eval(darg)) {
                | InvalidInput(msg) => Result.Error(msg)
                | BoxedValue(evald)
                | Indet(evald) => Result.Ok([(v, t, Some(evald)), ...acc])
                }
              }
            ),
          dargs,
          Result.Ok([]),
        );
      switch (sim_res, dargs_res) {
      | (Error(msg), _)
      | (_, Error(msg)) => InvalidInput(msg)
      | (Ok(sim), Ok(dargs)) =>
        let splice_info = SpliceInfo.update_splice_map(splice_info, sim);
        let dargs_opt' =
          dargs
          |> ListUtil.map_with_accumulator_opt(
               ((), (v, t, arg_opt)) =>
                 arg_opt |> OptUtil.map(arg => ((), (v, t, arg))),
               (),
             );
        switch (dargs_opt') {
        | Some(((), dargs')) when eval_livelit_holes =>
          // subst dargs before eval
          let model =
            dargs'
            |> List.fold_left(
                 (model, (v, _, darg)) => Exp.subst_var(darg, v, model),
                 model,
               );
          // subst splices before eval
          let model_res =
            Result.Ok(model)
            |> NatMap.fold(sim, (model_res, (name, (_, to_subst_opt))) =>
                 Result.bind(model_res, model =>
                   switch (to_subst_opt) {
                   | None => Result.Error(9)
                   | Some(to_subst) =>
                     Result.Ok(
                       Exp.subst_var(
                         to_subst,
                         SpliceInfo.var_of_splice_name(name),
                         model,
                       ),
                     )
                   }
                 )
               );
          switch (model_res) {
          | Result.Error(msg) => InvalidInput(msg)
          | Result.Ok(model) => _eval(model)
          };
        | _ =>
          Indet(LivelitHole(su, si, sigma, lln, splice_info, dargs, model))
        };
      };
    | Cast(d1, ty, ty') =>
      switch (_eval(d1)) {
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
          _eval(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          _eval(d');
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
          _eval(d');
        | (NotGroundOrHole(ty_grounded), Hole) =>
          /* ITGround rule */
          let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
          _eval(d');
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
      switch (_eval(d1)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d1')
      | Indet(d1') => Indet(FailedCast(d1', ty, ty'))
      }
    | InvalidOperation(d, err) =>
      switch (_eval(d)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d')
      | Indet(d') => Indet(InvalidOperation(d', err))
      }
    };
  }
  and evaluate_case =
      (
        ~eval_livelit_holes=false,
        inconsistent_info,
        scrut: DHExp.t,
        rules: list(DHExp.rule),
        current_rule_index: int,
      )
      : result =>
    switch (evaluate(~eval_livelit_holes, scrut)) {
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
        | Matches(env) => evaluate(~eval_livelit_holes, Exp.subst(env, d))
        | DoesNotMatch =>
          evaluate_case(
            ~eval_livelit_holes,
            inconsistent_info,
            scrut,
            rules,
            current_rule_index + 1,
          )
        }
      }
    };
};
