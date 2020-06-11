/* hole instance numbers are all 0 after expansion and during evaluation --
 * renumbering is done on the final result (see below) */

module Pat = {
  module ElaborationResult = {
    type t =
      | Elaborates(DHPat.t, HTyp.t, Contexts.t, Delta.t)
      | DoesNotElaborate;

    let to_option =
      fun
      | DoesNotElaborate => None
      | Elaborates(pat, ty, ctx, delta) => Some((pat, ty, ctx, delta));

    let from_option =
      fun
      | None => DoesNotElaborate
      | Some((pat, ty, ctx, delta)) => Elaborates(pat, ty, ctx, delta);

    let bind = (x: t, ~f: ((DHPat.t, HTyp.t, Contexts.t, Delta.t)) => t): t =>
      switch (x) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp, ty, ctx, delta) => f((dp, ty, ctx, delta))
      };
  };

  module Let_syntax = ElaborationResult;

  let rec syn_elab =
          (ctx: Contexts.t, delta: Delta.t, p: UHPat.t): ElaborationResult.t =>
    syn_elab_opseq(ctx, delta, p)
  and syn_elab_opseq =
      (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHPat.opseq)
      : ElaborationResult.t =>
    syn_elab_skel(ctx, delta, skel, seq)
  and syn_elab_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHPat.skel, seq: UHPat.seq)
      : ElaborationResult.t =>
    switch (skel) {
    | Placeholder(n) =>
      syn_elab_operand(ctx, delta, seq |> Seq.nth_operand(n))
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Elaborates(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => ElaborationResult.DoesNotElaborate
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHPat.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, ctx, delta) =
          syn_elab_skel(ctx, delta, skel1, seq);
        let%bind (dp2, ty2, ctx, delta) =
          syn_elab_skel(ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, ctx, delta), skel) => {
               syn_elab_skel(ctx, delta, skel, seq)
               |> ElaborationResult.to_option
               |> Option.map(((dp, ty, ctx, delta)) =>
                    ((DHPat.Pair(dp_acc, dp), ctx, delta), ty)
                  )
             },
             (DHPat.Pair(dp1, dp2), ctx, delta),
           )
        |> Option.map((((dp_acc, ctx, delta), tys)) =>
             (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), ctx, delta)
           )
        |> ElaborationResult.from_option;
      | _ =>
        raise(
          Invalid_argument(
            "Encountered tuple pattern type with less than 2 elements!",
          ),
        )
      }
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_elab_skel(ctx, delta, skel1, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        switch (syn_elab_skel(ctx, delta, skel2, seq)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(dp2, _, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Elaborates(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_elab_skel(ctx, delta, skel1, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, ty1, ctx, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(dp2, _, ctx, delta) =>
          let dp = DHPat.Cons(dp1, dp2);
          Elaborates(dp, ty, ctx, delta);
        };
      }
    }
  and syn_elab_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand)
      : ElaborationResult.t =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | StringLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp, _, ctx, delta) =>
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, Hole, gamma)));
        Elaborates(NonEmptyHole(reason, u, 0, dp), Hole, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => ElaborationResult.DoesNotElaborate
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = DHPat.EmptyHole(u, 0);
      let ty = HTyp.Hole;
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Elaborates(dp, ty, ctx, delta);
    | Wild(NotInHole) => Elaborates(Wild, Hole, ctx, delta)
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Elaborates(Keyword(u, 0, k), Hole, ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, Hole));
      Elaborates(Var(x), Hole, ctx, delta);
    | IntLit(NotInHole, n) =>
      switch (int_of_string_opt(n)) {
      | Some(n) => Elaborates(IntLit(n), Int, ctx, delta)
      | None => DoesNotElaborate
      }
    | FloatLit(NotInHole, f) =>
      switch (TextShape.hazel_float_of_string_opt(f)) {
      | Some(f) => Elaborates(FloatLit(f), Float, ctx, delta)
      | None => DoesNotElaborate
      }
    | BoolLit(NotInHole, b) => Elaborates(BoolLit(b), Bool, ctx, delta)
    | StringLit(NotInHole, s) =>
      Elaborates(StringLit(s), String, ctx, delta)
    | ListNil(NotInHole) => Elaborates(ListNil, List(Hole), ctx, delta)
    | Parenthesized(p1) => syn_elab(ctx, delta, p1)
    | Inj(NotInHole, side, p) =>
      switch (syn_elab(ctx, delta, p)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, ty1, ctx, delta) =>
        let dp = DHPat.Inj(side, dp1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Elaborates(dp, ty, ctx, delta);
      }
    }
  and ana_elab =
      (ctx: Contexts.t, delta: Delta.t, p: UHPat.t, ty: HTyp.t)
      : ElaborationResult.t =>
    ana_elab_opseq(ctx, delta, p, ty)
  and ana_elab_opseq =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        OpSeq(skel, seq) as opseq: UHPat.opseq,
        ty: HTyp.t,
      )
      : ElaborationResult.t => {
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
               switch (ana_elab_skel(ctx, delta, skel, seq, ty)) {
               | ElaborationResult.DoesNotElaborate => None
               | Elaborates(dp, _, ctx, delta) =>
                 Some(([dp, ...rev_dps], ctx, delta))
               }
             },
           Some(([], ctx, delta)),
         )
      |> (
        fun
        | None => ElaborationResult.DoesNotElaborate
        | Some((rev_dps, ctx, delta)) => {
            let dp = rev_dps |> List.rev |> DHPat.mk_tuple;
            Elaborates(dp, ty, ctx, delta);
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
                 switch (syn_elab_skel(ctx, delta, skel, seq)) {
                 | DoesNotElaborate => None
                 | Elaborates(dp, _, ctx, delta) =>
                   Some(([dp, ...rev_dps], ctx, delta))
                 }
               },
             Some(([], ctx, delta)),
           )
        |> (
          fun
          | None => ElaborationResult.DoesNotElaborate
          | Some((rev_dps, ctx, delta)) => {
              let dp = DHPat.mk_tuple(List.rev(rev_dps));
              Elaborates(dp, ty, ctx, delta);
            }
        );
      } else {
        switch (opseq |> UHPat.get_err_status_opseq) {
        | NotInHole
        | InHole(TypeInconsistent, _) => ElaborationResult.DoesNotElaborate
        | InHole(WrongLength as reason, u) =>
          switch (
            syn_elab_opseq(
              ctx,
              delta,
              opseq |> UHPat.set_err_status_opseq(NotInHole),
            )
          ) {
          | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(dp, _, _, delta) =>
            let gamma = ctx |> Contexts.gamma;
            let delta =
              MetaVarMap.extend_unique(
                delta,
                (u, (PatternHole, ty, gamma)),
              );
            Elaborates(NonEmptyHole(reason, u, 0, dp), ty, ctx, delta);
          }
        };
      }
    };
  }
  and ana_elab_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHPat.skel,
        seq: UHPat.seq,
        ty: HTyp.t,
      )
      : ElaborationResult.t =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      ElaborationResult.DoesNotElaborate
    | Placeholder(n) =>
      let pn = seq |> Seq.nth_operand(n);
      ana_elab_operand(ctx, delta, pn, ty);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Elaborates(dp, ty, ctx, delta);
      };
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, Hole)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, _ty1, ctx, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, Hole)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(dp2, _ty2, ctx, delta) =>
          let dp = DHPat.Ap(dp1, dp2);
          Elaborates(dp, Hole, ctx, delta);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some(ty_elt) =>
        switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(dp1, _, ctx, delta) =>
          let ty_list = HTyp.List(ty_elt);
          switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list)) {
          | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(dp2, _, ctx, delta) =>
            let dp = DHPat.Cons(dp1, dp2);
            Elaborates(dp, ty, ctx, delta);
          };
        }
      }
    }
  and ana_elab_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHPat.operand, ty: HTyp.t)
      : ElaborationResult.t =>
    switch (operand) {
    | Wild(InHole(TypeInconsistent as reason, u))
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | StringLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Inj(InHole(TypeInconsistent as reason, u), _, _) =>
      let operand' = operand |> UHPat.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp1, _, ctx, delta) =>
        let dp = DHPat.NonEmptyHole(reason, u, 0, dp1);
        let gamma = Contexts.gamma(ctx);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
        Elaborates(dp, ty, ctx, delta);
      };
    | Wild(InHole(WrongLength, _))
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Inj(InHole(WrongLength, _), _, _) => ElaborationResult.DoesNotElaborate
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let dp = DHPat.EmptyHole(u, 0);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (PatternHole, ty, gamma)));
      Elaborates(dp, ty, ctx, delta);
    | Var(NotInHole, InVarHole(Free, _), _) => raise(UHPat.FreeVarInPat)
    | Var(NotInHole, InVarHole(Keyword(k), u), _) =>
      Elaborates(Keyword(u, 0, k), ty, ctx, delta)
    | Var(NotInHole, NotInVarHole, x) =>
      let ctx = Contexts.extend_gamma(ctx, (x, ty));
      Elaborates(Var(x), ty, ctx, delta);
    | Wild(NotInHole) => Elaborates(Wild, ty, ctx, delta)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | BoolLit(NotInHole, _)
    | StringLit(NotInHole, _) => syn_elab_operand(ctx, delta, operand)
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some(ty_elt) => Elaborates(ListNil, HTyp.List(ty_elt), ctx, delta)
      }
    | Parenthesized(p) => ana_elab(ctx, delta, p, ty)
    | Inj(NotInHole, side, p1) =>
      switch (HTyp.matched_sum(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some((tyL, tyR)) =>
        let ty1 = InjSide.pick(side, tyL, tyR);
        switch (ana_elab(ctx, delta, p1, ty1)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(dp1, ty1, ctx, delta) =>
          let ty =
            switch (side) {
            | L => HTyp.Sum(ty1, tyR)
            | R => HTyp.Sum(tyL, ty1)
            };
          Elaborates(Inj(side, dp1), ty, ctx, delta);
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
    | StringLit(_)
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
    | BuiltInLit(y) =>
      if (Var.eq(x, y)) {
        d1;
      } else {
        d2;
      }
    | FailedAssert(_)
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
    | Subscript(d3, d4, d5) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      let d5 = subst_var(d1, x, d5);
      Subscript(d3, d4, d5);
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
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
    | BinStrOp(op, d3, d4) =>
      let d3 = subst_var(d1, x, d3);
      let d4 = subst_var(d1, x, d4);
      BinStrOp(op, d3, d4);
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
    | (_, Subscript(_, _, _)) => Indet
    | (_, BinBoolOp(_, _, _)) => Indet
    | (_, BinStrOp(_, _, _)) => Indet
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
    | (StringLit(n1), StringLit(n2)) =>
      if (n1 == n2) {
        Matches(Environment.empty);
      } else {
        DoesNotMatch;
      }
    | (StringLit(_), Cast(d, String, Hole)) => matches(dp, d)
    | (StringLit(_), Cast(d, Hole, String)) => matches(dp, d)
    | (StringLit(_), _) => DoesNotMatch
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
    | BuiltInLit(_) => DoesNotMatch
    | FailedAssert(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Subscript(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BinStrOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | StringLit(_) => DoesNotMatch
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
    | BuiltInLit(_) => DoesNotMatch
    | FailedAssert(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Subscript(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BinStrOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | StringLit(_) => DoesNotMatch
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
    | BuiltInLit(_) => DoesNotMatch
    | FailedAssert(_) => DoesNotMatch
    | FreeVar(_, _, _, _) => Indet
    | Keyword(_, _, _, _) => Indet
    | Let(_, _, _) => Indet
    | FixF(_, _, _) => DoesNotMatch
    | Lam(_, _, _) => DoesNotMatch
    | Subscript(_, _, _) => DoesNotMatch
    | Ap(_, _) => Indet
    | BinBoolOp(_, _, _)
    | BinIntOp(_, _, _)
    | BinFloatOp(_, _, _)
    | BinStrOp(_, _, _)
    | BoolLit(_) => DoesNotMatch
    | IntLit(_) => DoesNotMatch
    | FloatLit(_) => DoesNotMatch
    | StringLit(_) => DoesNotMatch
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

  type elab_result_lines =
    | LinesElaboration(DHExp.t => DHExp.t, Contexts.t, Delta.t)
    | LinesDoNotElaboration;

  module ElaborationResult = {
    type t =
      | Elaborates(DHExp.t, HTyp.t, Delta.t)
      | DoesNotElaborate;

    let to_option =
      fun
      | DoesNotElaborate => None
      | Elaborates(pat, ty, delta) => Some((pat, ty, delta));

    let from_option =
      fun
      | None => DoesNotElaborate
      | Some((pat, ty, delta)) => Elaborates(pat, ty, delta);

    let bind = (x: t, ~f: ((DHExp.t, HTyp.t, Delta.t)) => t): t =>
      switch (x) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(dp, ty, delta) => f((dp, ty, delta))
      };
  };

  module Let_syntax = ElaborationResult;

  let id_env = (ctx: VarCtx.t): Environment.t =>
    VarMap.map(
      xt => {
        let (x, _) = xt;
        DHExp.BoundVar(x);
      },
      ctx,
    );

  let rec syn_elab =
          (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): ElaborationResult.t =>
    syn_elab_block(ctx, delta, e)
  and syn_elab_block =
      (ctx: Contexts.t, delta: Delta.t, block: UHExp.block)
      : ElaborationResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ElaborationResult.DoesNotElaborate
    | Some((leading, conclusion)) =>
      switch (syn_elab_lines(ctx, delta, leading)) {
      | LinesDoNotElaboration => ElaborationResult.DoesNotElaborate
      | LinesElaboration(prelude, ctx, delta) =>
        switch (syn_elab_opseq(ctx, delta, conclusion)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d, ty, delta) => Elaborates(prelude(d), ty, delta)
        }
      }
    }
  and syn_elab_lines =
      (ctx: Contexts.t, delta: Delta.t, lines: list(UHExp.line))
      : elab_result_lines =>
    switch (lines) {
    | [] => LinesElaboration(d => d, ctx, delta)
    | [line, ...lines] =>
      switch (syn_elab_line(ctx, delta, line)) {
      | LinesDoNotElaboration => LinesDoNotElaboration
      | LinesElaboration(prelude_line, ctx, delta) =>
        switch (syn_elab_lines(ctx, delta, lines)) {
        | LinesDoNotElaboration => LinesDoNotElaboration
        | LinesElaboration(prelude_lines, ctx, delta) =>
          LinesElaboration(d => prelude_line(prelude_lines(d)), ctx, delta)
        }
      }
    }
  and syn_elab_line =
      (ctx: Contexts.t, delta: Delta.t, line: UHExp.line): elab_result_lines =>
    switch (line) {
    | ExpLine(e1) =>
      switch (syn_elab_opseq(ctx, delta, e1)) {
      | ElaborationResult.DoesNotElaborate => LinesDoNotElaboration
      | Elaborates(d1, _, delta) =>
        let prelude = d2 => DHExp.Let(Wild, d1, d2);
        LinesElaboration(prelude, ctx, delta);
      }
    | EmptyLine => LinesElaboration(d => d, ctx, delta)
    | LetLine(p, ann, def) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let (ctx1, is_recursive_fn) =
          Statics.Exp.ctx_for_let'(ctx, p, ty1, def);
        switch (ana_elab(ctx1, delta, def, ty1)) {
        | ElaborationResult.DoesNotElaborate => LinesDoNotElaboration
        | Elaborates(d1, ty1', delta) =>
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
          switch (Pat.ana_elab(ctx, delta, p, ty1)) {
          | Pat.ElaborationResult.DoesNotElaborate => LinesDoNotElaboration
          | Elaborates(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesElaboration(prelude, ctx, delta);
          };
        };
      | None =>
        switch (syn_elab(ctx, delta, def)) {
        | ElaborationResult.DoesNotElaborate => LinesDoNotElaboration
        | Elaborates(d1, ty1, delta) =>
          switch (Pat.ana_elab(ctx, delta, p, ty1)) {
          | Pat.ElaborationResult.DoesNotElaborate => LinesDoNotElaboration
          | Elaborates(dp, _, ctx, delta) =>
            let prelude = d2 => DHExp.Let(dp, d1, d2);
            LinesElaboration(prelude, ctx, delta);
          }
        }
      }
    }
  and syn_elab_opseq =
      (ctx: Contexts.t, delta: Delta.t, OpSeq(skel, seq): UHExp.opseq)
      : ElaborationResult.t =>
    syn_elab_skel(ctx, delta, skel, seq)
  and syn_elab_skel =
      (ctx: Contexts.t, delta: Delta.t, skel: UHExp.skel, seq: UHExp.seq)
      : ElaborationResult.t =>
    switch (skel) {
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      syn_elab_operand(ctx, delta, en);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
    | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, Hole, gamma)),
          );
        Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      };
    | BinOp(InHole(WrongLength, _), _, _, _) => ElaborationResult.DoesNotElaborate
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (Statics.Exp.syn_skel(ctx, skel1, seq)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some(ty1) =>
        switch (HTyp.matched_arrow(ty1)) {
        | None => ElaborationResult.DoesNotElaborate
        | Some((ty2, ty)) =>
          let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
          switch (ana_elab_skel(ctx, delta, skel1, seq, ty2_arrow_ty)) {
          | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(d1, ty1', delta) =>
            switch (ana_elab_skel(ctx, delta, skel2, seq, ty2)) {
            | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
            | Elaborates(d2, ty2', delta) =>
              let dc1 = DHExp.cast(d1, ty1', ty2_arrow_ty);
              let dc2 = DHExp.cast(d2, ty2', ty2);
              let d = DHExp.Ap(dc1, dc2);
              Elaborates(d, ty, delta);
            }
          };
        }
      }
    | BinOp(NotInHole, Comma, _, _) =>
      switch (UHExp.get_tuple_elements(skel)) {
      | [skel1, skel2, ...tail] =>
        let%bind (dp1, ty1, delta) = syn_elab_skel(ctx, delta, skel1, seq);
        let%bind (dp2, ty2, delta) = syn_elab_skel(ctx, delta, skel2, seq);
        tail
        |> ListUtil.map_with_accumulator_opt(
             ((dp_acc, delta), skel) => {
               syn_elab_skel(ctx, delta, skel, seq)
               |> ElaborationResult.to_option
               |> Option.map(((dp, ty, delta)) =>
                    ((DHExp.Pair(dp_acc, dp), delta), ty)
                  )
             },
             (DHExp.Pair(dp1, dp2), delta),
           )
        |> Option.map((((dp_acc, delta), tys)) =>
             (dp_acc, HTyp.Prod([ty1, ty2, ...tys]), delta)
           )
        |> ElaborationResult.from_option;
      | _ =>
        raise(
          Invalid_argument(
            "Encountered tuple pattern type with less than 2 elements!",
          ),
        )
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_elab_skel(ctx, delta, skel1, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        let ty = HTyp.List(ty1);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          let d2c = DHExp.cast(d2, ty2, ty);
          let d = DHExp.Cons(d1, d2c);
          Elaborates(d, ty, delta);
        };
      }
    | BinOp(NotInHole, (Plus | Minus | Times | Divide) as op, skel1, skel2)
    | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, Int)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, Int)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Int);
          let dc2 = DHExp.cast(d2, ty2, Int);
          switch (DHExp.BinIntOp.of_op(op)) {
          | None => ElaborationResult.DoesNotElaborate
          | Some((op, ty)) =>
            let d = DHExp.BinIntOp(op, dc1, dc2);
            Elaborates(d, ty, delta);
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
      switch (ana_elab_skel(ctx, delta, skel1, seq, Float)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, Float)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Float);
          let dc2 = DHExp.cast(d2, ty2, Float);
          switch (DHExp.BinFloatOp.of_op(op)) {
          | None => DoesNotElaborate
          | Some((op, ty)) =>
            let d = DHExp.BinFloatOp(op, dc1, dc2);
            Elaborates(d, ty, delta);
          };
        }
      }
    | BinOp(NotInHole, Caret as op, skel1, skel2) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, String)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, String)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, String);
          let dc2 = DHExp.cast(d2, ty2, String);
          switch (DHExp.BinStrOp.of_op(op)) {
          | None => DoesNotElaborate
          | Some((op, ty)) =>
            let d = DHExp.BinStrOp(op, dc1, dc2);
            Elaborates(d, ty, delta);
          };
        }
      }
    | BinOp(NotInHole, (And | Or) as op, skel1, skel2) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, Bool)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (ana_elab_skel(ctx, delta, skel2, seq, Bool)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          let dc1 = DHExp.cast(d1, ty1, Bool);
          let dc2 = DHExp.cast(d2, ty2, Bool);
          switch (DHExp.BinBoolOp.of_op(op)) {
          | None => ElaborationResult.DoesNotElaborate
          | Some(op) =>
            let d = DHExp.BinBoolOp(op, dc1, dc2);
            Elaborates(d, Bool, delta);
          };
        }
      }
    }
  and syn_elab_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHExp.operand)
      : ElaborationResult.t =>
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | StringLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _)
    | Subscript(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(
            delta,
            (u, (ExpressionHole, Hole, gamma)),
          );
        Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      };
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _)
    | Subscript(InHole(WrongLength, _), _, _, _) => ElaborationResult.DoesNotElaborate
    | Case(InconsistentBranches(rule_types, u), scrut, rules) =>
      switch (syn_elab(ctx, delta, scrut)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, pat_ty, delta) =>
        let elab_rules =
          List.fold_left2(
            (b, r_t, r) =>
              switch (b) {
              | None => None
              | Some((drs, delta)) =>
                switch (syn_elab_rule(ctx, delta, r, pat_ty, r_t)) {
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
        switch (elab_rules) {
        | None => DoesNotElaborate
        | Some((drs, delta)) =>
          let gamma = Contexts.gamma(ctx);
          let sigma = id_env(gamma);
          let delta =
            MetaVarMap.extend_unique(
              delta,
              (u, (ExpressionHole, Hole, gamma)),
            );
          let d = DHExp.Case(d1, drs, 0);
          Elaborates(InconsistentBranches(u, 0, sigma, d), Hole, delta);
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
      Elaborates(d, ty, delta);
    | Var(NotInHole, NotInVarHole, x) =>
      let gamma = Contexts.gamma(ctx);
      switch (VarMap.lookup(gamma, x), BuiltinFunctions.lookup(x)) {
      | (Some(ty'), Some(ty)) =>
        print_endline("Dynamics1280");
        /* if (List.mem(x, BuiltinFunctions.shadowing_var) == true) { */
        if (HTyp.is_Arrow(ty') == false) {
          print_endline("Dynamics1282");
          Elaborates(BoundVar(x), ty', delta);
        } else {
          /* TODO: fix this with self-defined functions */
          Elaborates(
            BuiltInLit(x),
            ty,
            delta,
          );
        };
      /* } else {
            Elaborates(BuiltInLit(x), ty, delta);
         } */
      | (Some(ty), _) =>
        print_endline("Dynamics1295");
        Elaborates(BoundVar(x), ty, delta);
      | (None, _) => ElaborationResult.DoesNotElaborate
      };
    | Var(NotInHole, InVarHole(reason, u), x) =>
      // switch (BuiltinFunctions.builtinlookup(x)) {
      // | Some(ty) => Elaborates(BuiltInLit(x), ty, delta)
      // | None =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, Hole, gamma)));
      let d =
        switch (reason) {
        | Free => DHExp.FreeVar(u, 0, sigma, x)
        | Keyword(k) => DHExp.Keyword(u, 0, sigma, k)
        };
      Elaborates(d, Hole, delta);
    // }
    | IntLit(NotInHole, n) =>
      switch (int_of_string_opt(n)) {
      | Some(n) => Elaborates(IntLit(n), Int, delta)
      | None => DoesNotElaborate
      }
    | FloatLit(NotInHole, f) =>
      switch (TextShape.hazel_float_of_string_opt(f)) {
      | Some(f) => Elaborates(FloatLit(f), Float, delta)
      | None => DoesNotElaborate
      }
    | BoolLit(NotInHole, b) => Elaborates(BoolLit(b), Bool, delta)
    | StringLit(NotInHole, s) => Elaborates(StringLit(s), String, delta)
    | ListNil(NotInHole) =>
      let elt_ty = HTyp.Hole;
      Elaborates(ListNil(elt_ty), List(elt_ty), delta);
    | Parenthesized(body) => syn_elab(ctx, delta, body)
    | Lam(NotInHole, p, ann, body) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => HTyp.Hole
        };
      switch (Pat.ana_elab(ctx, delta, p, ty1)) {
      | Pat.ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(dp, _, ctx, delta) =>
        switch (syn_elab(ctx, delta, body)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d1, ty2, delta) =>
          let d = DHExp.Lam(dp, ty1, d1);
          Elaborates(d, Arrow(ty1, ty2), delta);
        }
      };
    | Inj(NotInHole, side, body) =>
      switch (syn_elab(ctx, delta, body)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        let d = DHExp.Inj(Hole, side, d1);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        Elaborates(d, ty, delta);
      }
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      switch (syn_elab(ctx, delta, scrut)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty, delta) =>
        switch (syn_elab_rules(ctx, delta, rules, ty)) {
        | None => DoesNotElaborate
        | Some((drs, glb, delta)) =>
          let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
          Elaborates(d, glb, delta);
        }
      }
    | Subscript(NotInHole, body1, body2, body3) =>
      switch (syn_elab(ctx, delta, body1)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (syn_elab(ctx, delta, body2)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d2, ty2, delta) =>
          switch (syn_elab(ctx, delta, body3)) {
          | DoesNotElaborate => DoesNotElaborate
          | Elaborates(d3, ty3, delta) =>
            let dc1 = DHExp.cast(d1, ty1, String);
            let dc2 = DHExp.cast(d2, ty2, Int);
            let dc3 = DHExp.cast(d3, ty3, Int);
            let d = DHExp.Subscript(dc1, dc2, dc3);
            Elaborates(d, String, delta);
          }
        }
      }
    | ApPalette(NotInHole, _name, _serialized_model, _hole_data) => ElaborationResult.DoesNotElaborate
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
         ana_elab_exp ctx bound_expansion expansion_ty
       | None -> ElaborationResult.DoesNotElaborate
       end */
    }
  and syn_elab_rules =
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
      let elabed_rule_info =
        List.fold_left(
          (b, r) =>
            switch (b) {
            | None => None
            | Some((drs, delta)) =>
              switch (syn_elab_rule(ctx, delta, r, pat_ty, glb)) {
              | None => None
              | Some((dr, delta)) =>
                let drs = drs @ [dr];
                Some((drs, delta));
              }
            },
          Some(([], delta)),
          rules,
        );
      switch (elabed_rule_info) {
      | None => None
      | Some((drs, delta)) => Some((drs, glb, delta))
      };
    }
  and syn_elab_rule =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        r: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((DHExp.rule, Delta.t)) => {
    let UHExp.Rule(p, clause) = r;
    switch (Pat.ana_elab(ctx, delta, p, pat_ty)) {
    | DoesNotElaborate => None
    | Elaborates(dp, _, ctx, delta) =>
      switch (syn_elab(ctx, delta, clause)) {
      | DoesNotElaborate => None
      | Elaborates(d1, ty1, delta) =>
        Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta))
      }
    };
  }
  and ana_elab =
      (ctx: Contexts.t, delta: Delta.t, e: UHExp.t, ty: HTyp.t)
      : ElaborationResult.t =>
    ana_elab_block(ctx, delta, e, ty)
  and ana_elab_block =
      (ctx: Contexts.t, delta: Delta.t, block: UHExp.block, ty: HTyp.t)
      : ElaborationResult.t =>
    switch (block |> UHExp.Block.split_conclusion) {
    | None => ElaborationResult.DoesNotElaborate
    | Some((leading, conclusion)) =>
      switch (syn_elab_lines(ctx, delta, leading)) {
      | LinesDoNotElaboration => ElaborationResult.DoesNotElaborate
      | LinesElaboration(prelude, ctx, delta) =>
        switch (ana_elab_opseq(ctx, delta, conclusion, ty)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d, ty, delta) => Elaborates(prelude(d), ty, delta)
        }
      }
    }
  and ana_elab_opseq =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        OpSeq(skel, seq) as opseq: UHExp.opseq,
        ty: HTyp.t,
      )
      : ElaborationResult.t => {
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
               switch (ana_elab_skel(ctx, delta, skel, seq, ty)) {
               | ElaborationResult.DoesNotElaborate => None
               | Elaborates(d, ty, delta) =>
                 Some(([d, ...rev_ds], [ty, ...rev_tys], delta))
               }
             },
           Some(([], [], delta)),
         )
      |> (
        fun
        | None => ElaborationResult.DoesNotElaborate
        | Some((rev_ds, rev_tys, delta)) => {
            let d = rev_ds |> List.rev |> DHExp.mk_tuple;
            let ty =
              switch (rev_tys) {
              | [] => failwith("expected at least 1 element")
              | [ty] => ty
              | _ => HTyp.Prod(rev_tys |> List.rev)
              };
            Elaborates(d, ty, delta);
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
                 switch (syn_elab_skel(ctx, delta, skel, seq)) {
                 | DoesNotElaborate => None
                 | Elaborates(d, ty, delta) =>
                   Some(([d, ...rev_ds], [ty, ...rev_tys], delta))
                 }
               },
             Some(([], [], delta)),
           )
        |> (
          fun
          | None => ElaborationResult.DoesNotElaborate
          | Some((rev_ds, rev_tys, delta)) => {
              let d = DHExp.mk_tuple(List.rev(rev_ds));
              let ty =
                switch (rev_tys) {
                | [] => failwith("expected at least 1 element")
                | [ty] => ty
                | _ => HTyp.Prod(rev_tys |> List.rev)
                };
              Elaborates(d, ty, delta);
            }
        );
      } else {
        switch (opseq |> UHExp.get_err_status_opseq) {
        | NotInHole
        | InHole(TypeInconsistent, _) => ElaborationResult.DoesNotElaborate
        | InHole(WrongLength as reason, u) =>
          switch (
            syn_elab_opseq(
              ctx,
              delta,
              opseq |> UHExp.set_err_status_opseq(NotInHole),
            )
          ) {
          | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(d, _, delta) =>
            let gamma = ctx |> Contexts.gamma;
            let sigma = gamma |> id_env;
            let delta =
              MetaVarMap.extend_unique(
                delta,
                (u, (ExpressionHole, ty, gamma)),
              );
            Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
          }
        };
      }
    };
  }
  and ana_elab_skel =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        skel: UHExp.skel,
        seq: UHExp.seq,
        ty: HTyp.t,
      )
      : ElaborationResult.t => {
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      // tuples handled at opseq level
      ElaborationResult.DoesNotElaborate
    | Placeholder(n) =>
      let en = seq |> Seq.nth_operand(n);
      ana_elab_operand(ctx, delta, en, ty);
    | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1);
        Elaborates(d, Hole, delta);
      };
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some(ty_elt) =>
        switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d1, ty_elt', delta) =>
          let d1c = DHExp.cast(d1, ty_elt', ty_elt);
          let ty_list = HTyp.List(ty_elt);
          switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list)) {
          | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(d2, ty2, delta) =>
            let d2c = DHExp.cast(d2, ty2, ty_list);
            let d = DHExp.Cons(d1c, d2c);
            Elaborates(d, ty_list, delta);
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
        Caret |
        Space,
        _,
        _,
      ) =>
      switch (syn_elab_skel(ctx, delta, skel, seq)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d, ty', delta) =>
        if (HTyp.consistent(ty, ty')) {
          Elaborates(d, ty', delta);
        } else {
          ElaborationResult.DoesNotElaborate;
        }
      }
    };
  }
  and ana_elab_operand =
      (ctx: Contexts.t, delta: Delta.t, operand: UHExp.operand, ty: HTyp.t)
      : ElaborationResult.t => {
    switch (operand) {
    /* in hole */
    | Var(InHole(TypeInconsistent as reason, u), _, _)
    | IntLit(InHole(TypeInconsistent as reason, u), _)
    | FloatLit(InHole(TypeInconsistent as reason, u), _)
    | BoolLit(InHole(TypeInconsistent as reason, u), _)
    | StringLit(InHole(TypeInconsistent as reason, u), _)
    | ListNil(InHole(TypeInconsistent as reason, u))
    | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
    | Inj(InHole(TypeInconsistent as reason, u), _, _)
    | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
    | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _)
    | Subscript(InHole(TypeInconsistent as reason, u), _, _, _) =>
      let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
      switch (syn_elab_operand(ctx, delta, operand')) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d, _, delta) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = id_env(gamma);
        let delta =
          MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
        Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
      };
    | Var(InHole(WrongLength, _), _, _)
    | IntLit(InHole(WrongLength, _), _)
    | FloatLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | StringLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _))
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(
        StandardErrStatus(InHole(WrongLength, _)) |
        InconsistentBranches(_, _),
        _,
        _,
      )
    | ApPalette(InHole(WrongLength, _), _, _, _)
    | Subscript(InHole(WrongLength, _), _, _, _) => ElaborationResult.DoesNotElaborate
    /* not in hole */
    | EmptyHole(u) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let d = DHExp.EmptyHole(u, 0, sigma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      Elaborates(d, ty, delta);
    | Var(NotInHole, InVarHole(reason, u), x) =>
      // switch (BuiltinFunctions.builtinlookup(x)) {
      // | Some(ty) => Elaborates(BuiltInLit(x), ty, delta)
      // | None =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.extend_unique(delta, (u, (ExpressionHole, ty, gamma)));
      let d: DHExp.t =
        switch (reason) {
        | Free => FreeVar(u, 0, sigma, x)
        | Keyword(k) => Keyword(u, 0, sigma, k)
        };
      Elaborates(d, ty, delta);
    // }
    | Parenthesized(body) => ana_elab(ctx, delta, body, ty)
    | Lam(NotInHole, p, ann, body) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          switch (HTyp.consistent(ty1_ann, ty1_given)) {
          | false => ElaborationResult.DoesNotElaborate
          | true =>
            switch (Pat.ana_elab(ctx, delta, p, ty1_ann)) {
            | Pat.ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
            | Elaborates(dp, ty1p, ctx, delta) =>
              switch (ana_elab(ctx, delta, body, ty2)) {
              | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
              | Elaborates(d1, ty2, delta) =>
                let ty = HTyp.Arrow(ty1p, ty2);
                let d = DHExp.Lam(dp, ty1p, d1);
                Elaborates(d, ty, delta);
              }
            }
          };
        | None =>
          switch (Pat.ana_elab(ctx, delta, p, ty1_given)) {
          | Pat.ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
          | Elaborates(dp, ty1, ctx, delta) =>
            switch (ana_elab(ctx, delta, body, ty2)) {
            | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
            | Elaborates(d1, ty2, delta) =>
              let ty = HTyp.Arrow(ty1, ty2);
              let d = DHExp.Lam(dp, ty1, d1);
              Elaborates(d, ty, delta);
            }
          }
        }
      }
    | Inj(NotInHole, side, body) =>
      switch (HTyp.matched_sum(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some((ty1, ty2)) =>
        let e1ty = InjSide.pick(side, ty1, ty2);
        switch (ana_elab(ctx, delta, body, e1ty)) {
        | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
        | Elaborates(d1, e1ty', delta) =>
          let (ann_ty, ty) =
            switch (side) {
            | L => (ty2, HTyp.Sum(e1ty', ty2))
            | R => (ty1, HTyp.Sum(ty1, e1ty'))
            };
          let d = DHExp.Inj(ann_ty, side, d1);
          Elaborates(d, ty, delta);
        };
      }
    | Case(StandardErrStatus(NotInHole), scrut, rules) =>
      switch (syn_elab(ctx, delta, scrut)) {
      | ElaborationResult.DoesNotElaborate => ElaborationResult.DoesNotElaborate
      | Elaborates(d1, ty1, delta) =>
        switch (ana_elab_rules(ctx, delta, rules, ty1, ty)) {
        | None => ElaborationResult.DoesNotElaborate
        | Some((drs, delta)) =>
          let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
          Elaborates(d, ty, delta);
        }
      }
    | ListNil(NotInHole) =>
      switch (HTyp.matched_list(ty)) {
      | None => ElaborationResult.DoesNotElaborate
      | Some(elt_ty) => Elaborates(ListNil(elt_ty), List(elt_ty), delta)
      }
    | Var(NotInHole, NotInVarHole, _)
    | BoolLit(NotInHole, _)
    | IntLit(NotInHole, _)
    | FloatLit(NotInHole, _)
    | StringLit(NotInHole, _) => syn_elab_operand(ctx, delta, operand)
    | ApPalette(NotInHole, _, _, _)
    | Subscript(NotInHole, _, _, _) =>
      /* subsumption */
      syn_elab_operand(ctx, delta, operand)
    };
  }
  and ana_elab_rules =
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
             switch (ana_elab_rule(ctx, delta, r, pat_ty, clause_ty)) {
             | None => None
             | Some((dr, delta)) =>
               let drs = drs @ [dr];
               Some((drs, delta));
             }
           },
         Some(([], delta)),
       )
  and ana_elab_rule =
      (
        ctx: Contexts.t,
        delta: Delta.t,
        r: UHExp.rule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option((DHExp.rule, Delta.t)) => {
    let UHExp.Rule(p, clause) = r;
    switch (Pat.ana_elab(ctx, delta, p, pat_ty)) {
    | Pat.ElaborationResult.DoesNotElaborate => None
    | Elaborates(dp, _, ctx, delta) =>
      switch (ana_elab(ctx, delta, clause, clause_ty)) {
      | ElaborationResult.DoesNotElaborate => None
      | Elaborates(d1, ty1, delta) =>
        Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta))
      }
    };
  };

  let rec renumber_result_only =
          (path: InstancePath.t, hii: HoleInstanceInfo.t, d: DHExp.t)
          : (DHExp.t, HoleInstanceInfo.t) =>
    switch (d) {
    | BoundVar(_)
    | BuiltInLit(_)
    | FailedAssert(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
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
    | Subscript(d1, d2, d3) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      let (d3, hii) = renumber_result_only(path, hii, d3);
      (Subscript(d1, d2, d3), hii);
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
    | BinStrOp(op, d1, d2) =>
      let (d1, hii) = renumber_result_only(path, hii, d1);
      let (d2, hii) = renumber_result_only(path, hii, d2);
      (BinStrOp(op, d1, d2), hii);
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
    | BuiltInLit(_)
    | FailedAssert(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
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
    | Subscript(d1, d2, d3) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      let (d3, hii) = renumber_sigmas_only(path, hii, d3);
      (Subscript(d1, d2, d3), hii);
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
    | BinStrOp(op, d1, d2) =>
      let (d1, hii) = renumber_sigmas_only(path, hii, d1);
      let (d2, hii) = renumber_sigmas_only(path, hii, d2);
      (BinStrOp(op, d1, d2), hii);
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
