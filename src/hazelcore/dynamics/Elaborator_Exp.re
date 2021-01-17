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
  | InvalidText(_) => d2
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
  | ConsistentIf(If(d3, d4, d5)) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    let d5 = subst_var(d1, x, d5);
    ConsistentIf(If(d3, d4, d5));
  | InconsistentBranchesIf(u, i, sigma, If(d3, d4, d5)) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    let d5 = subst_var(d1, x, d5);
    let sigma' = subst_var_env(d1, x, sigma);
    InconsistentBranchesIf(u, i, sigma', If(d3, d4, d5));
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
  | (InvalidText(_), _) => Indet
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_, _, _)) => Indet
  | (_, NonEmptyHole(_, _, _, _, _)) => Indet
  | (_, FailedCast(_, _, _)) => Indet
  | (_, InvalidOperation(_)) => Indet
  | (_, FreeVar(_, _, _, _)) => Indet
  | (_, InvalidText(_)) => Indet
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
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
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
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
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
  | InvalidText(_) => Indet
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
  | ConsistentIf(_)
  | InconsistentBranchesIf(_) => Indet
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
    | Indet =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
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
  | InvalidText(_) => Indet
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
  | ConsistentIf(_)
  | InconsistentBranchesIf(_) => Indet
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
    | Indet =>
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
      | Indet
      | Matches(_) => Indet
      };
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
  | InvalidText(_) => Indet
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
  | ConsistentIf(_)
  | InconsistentBranchesIf(_) => Indet
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  };

type elab_result_lines =
  | LinesExpand(DHExp.t => DHExp.t, Contexts.t, Delta.t)
  | LinesDoNotExpand;

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
    (ctx: Contexts.t, delta: Delta.t, block: UHExp.block): ElaborationResult.t =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None => DoesNotElaborate
  | Some((leading, conclusion)) =>
    switch (syn_elab_lines(ctx, delta, leading)) {
    | LinesDoNotExpand => DoesNotElaborate
    | LinesExpand(prelude, ctx, delta) =>
      switch (syn_elab_opseq(ctx, delta, conclusion)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d, ty, delta) => Elaborates(prelude(d), ty, delta)
      }
    }
  }
and syn_elab_lines =
    (ctx: Contexts.t, delta: Delta.t, lines: list(UHExp.line))
    : elab_result_lines =>
  switch (lines) {
  | [] => LinesExpand(d => d, ctx, delta)
  | [line, ...lines] =>
    switch (syn_elab_line(ctx, delta, line)) {
    | LinesDoNotExpand => LinesDoNotExpand
    | LinesExpand(prelude_line, ctx, delta) =>
      switch (syn_elab_lines(ctx, delta, lines)) {
      | LinesDoNotExpand => LinesDoNotExpand
      | LinesExpand(prelude_lines, ctx, delta) =>
        LinesExpand(d => prelude_line(prelude_lines(d)), ctx, delta)
      }
    }
  }
and syn_elab_line =
    (ctx: Contexts.t, delta: Delta.t, line: UHExp.line): elab_result_lines =>
  switch (line) {
  | ExpLine(e1) =>
    switch (syn_elab_opseq(ctx, delta, e1)) {
    | DoesNotElaborate => LinesDoNotExpand
    | Elaborates(d1, _, delta) =>
      let prelude = d2 => DHExp.Let(Wild, d1, d2);
      LinesExpand(prelude, ctx, delta);
    }
  | EmptyLine
  | CommentLine(_) => LinesExpand(d => d, ctx, delta)
  | LetLine(p, ann, def) =>
    switch (ann) {
    | Some(uty1) =>
      let ty1 = UHTyp.expand(uty1);
      let (ctx1, is_recursive_fn) =
        Statics_Exp.ctx_for_let(ctx, p, ty1, def);
      switch (ana_elab(ctx1, delta, def, ty1)) {
      | DoesNotElaborate => LinesDoNotExpand
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
        switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1)) {
        | DoesNotElaborate => LinesDoNotExpand
        | Elaborates(dp, _, ctx, delta) =>
          let prelude = d2 => DHExp.Let(dp, d1, d2);
          LinesExpand(prelude, ctx, delta);
        };
      };
    | None =>
      switch (syn_elab(ctx, delta, def)) {
      | DoesNotElaborate => LinesDoNotExpand
      | Elaborates(d1, ty1, delta) =>
        switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1)) {
        | DoesNotElaborate => LinesDoNotExpand
        | Elaborates(dp, _, ctx, delta) =>
          let prelude = d2 => DHExp.Let(dp, d1, d2);
          LinesExpand(prelude, ctx, delta);
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
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
    };
  | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (Statics_Exp.syn_skel(ctx, skel1, seq)) {
    | None => DoesNotElaborate
    | Some(ty1) =>
      switch (HTyp.matched_arrow(ty1)) {
      | None => DoesNotElaborate
      | Some((ty2, ty)) =>
        let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
        switch (ana_elab_skel(ctx, delta, skel1, seq, ty2_arrow_ty)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d1, ty1', delta) =>
          switch (ana_elab_skel(ctx, delta, skel2, seq, ty2)) {
          | DoesNotElaborate => DoesNotElaborate
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
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta) =>
      let ty = HTyp.List(ty1);
      switch (ana_elab_skel(ctx, delta, skel2, seq, ty)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, ty2, delta) =>
        let d2c = DHExp.cast(d2, ty2, ty);
        let d = DHExp.Cons(d1, d2c);
        Elaborates(d, ty, delta);
      };
    }
  | BinOp(NotInHole, (Plus | Minus | Times | Divide) as op, skel1, skel2)
  | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Int)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Int)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, ty2, delta) =>
        let dc1 = DHExp.cast(d1, ty1, Int);
        let dc2 = DHExp.cast(d2, ty2, Int);
        switch (DHExp.BinIntOp.of_op(op)) {
        | None => DoesNotElaborate
        | Some((op, ty)) =>
          let d = DHExp.BinIntOp(op, dc1, dc2);
          Elaborates(d, ty, delta);
        };
      }
    }
  | BinOp(NotInHole, (FPlus | FMinus | FTimes | FDivide) as op, skel1, skel2)
  | BinOp(NotInHole, (FLessThan | FGreaterThan | FEquals) as op, skel1, skel2) =>
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
  | BinOp(NotInHole, (And | Or) as op, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Bool)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Bool)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, ty2, delta) =>
        let dc1 = DHExp.cast(d1, ty1, Bool);
        let dc2 = DHExp.cast(d2, ty2, Bool);
        switch (DHExp.BinBoolOp.of_op(op)) {
        | None => DoesNotElaborate
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
  | ListNil(InHole(TypeInconsistent as reason, u))
  | Lam(InHole(TypeInconsistent as reason, u), _, _, _)
  | Inj(InHole(TypeInconsistent as reason, u), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
  | If(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _, _)
  | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
    };
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | If(StandardErrStatus(InHole(WrongLength, _)), _, _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
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
          MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
        let d = DHExp.Case(d1, drs, 0);
        Elaborates(InconsistentBranches(u, 0, sigma, d), Hole, delta);
      };
    }
  | If(InconsistentBranches(_, u), t1, t2, t3) =>
    switch (ana_elab(ctx, delta, t1, HTyp.Bool)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let then_branch = syn_elab(ctx, delta, t2);
      let else_branch = syn_elab(ctx, delta, t3);
      switch (then_branch, else_branch) {
      | (Elaborates(d2, _, delta), Elaborates(d3, _, _)) =>
        let delta =
          MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
        let d = DHExp.If(d1, d2, d3);
        Elaborates(InconsistentBranchesIf(u, 0, sigma, d), HTyp.Hole, delta);
      | (_, _) => DoesNotElaborate
      };
    }
  /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let ty = HTyp.Hole;
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let d = DHExp.InvalidText(u, 0, sigma, t);
    let ty = HTyp.Hole;
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | Var(NotInHole, NotInVarHole, x) =>
    let gamma = Contexts.gamma(ctx);
    switch (VarMap.lookup(gamma, x)) {
    | Some(ty) => Elaborates(BoundVar(x), ty, delta)
    | None => DoesNotElaborate
    };
  | Var(NotInHole, InVarHole(reason, u), x) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let delta =
      MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
    let d =
      switch (reason) {
      | Free => DHExp.FreeVar(u, 0, sigma, x)
      | Keyword(k) => DHExp.Keyword(u, 0, sigma, k)
      };
    Elaborates(d, Hole, delta);
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
    switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, _, ctx, delta) =>
      switch (syn_elab(ctx, delta, body)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty2, delta) =>
        let d = DHExp.Lam(dp, ty1, d1);
        Elaborates(d, Arrow(ty1, ty2), delta);
      }
    };
  | Inj(NotInHole, side, body) =>
    switch (syn_elab(ctx, delta, body)) {
    | DoesNotElaborate => DoesNotElaborate
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
  | If(StandardErrStatus(NotInHole), t1, t2, t3) =>
    switch (ana_elab(ctx, delta, t1, HTyp.Bool)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, _, delta) =>
      switch (syn_elab(ctx, delta, t2)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, _, delta) =>
        switch (syn_elab(ctx, delta, t3)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d3, ty, delta) =>
          let d = DHExp.ConsistentIf(If(d1, d2, d3));
          Elaborates(d, ty, delta);
        }
      }
    }
  | ApPalette(NotInHole, _name, _serialized_model, _hole_data) =>
    DoesNotElaborate /* let (_, palette_ctx) = ctx in
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
               let opseq = Seq.ExpOpExp (UHExp.Parenthesized lam) Operators_Exp.Space (UHExp.Parenthesized hexp_ann) in
               let ap = UHExp.OpSeq (UHExp.associate opseq) opseq in
               UHExp.Tm NotInHole ap
             )
             expansion in
       ana_elab_exp ctx bound_expansion expansion_ty
     | None -> DoesNotElaborate
     end */ /* TODO fix me */
  }
and syn_elab_rules =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      rules: list(UHExp.rule),
      pat_ty: HTyp.t,
    )
    : option((list(DHExp.rule), HTyp.t, Delta.t)) =>
  switch (Statics_Exp.syn_rules(ctx, rules, pat_ty)) {
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
  switch (Elaborator_Pat.ana_elab(ctx, delta, p, pat_ty)) {
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
  | None => DoesNotElaborate
  | Some((leading, conclusion)) =>
    switch (syn_elab_lines(ctx, delta, leading)) {
    | LinesDoNotExpand => DoesNotElaborate
    | LinesExpand(prelude, ctx, delta) =>
      switch (ana_elab_opseq(ctx, delta, conclusion, ty)) {
      | DoesNotElaborate => DoesNotElaborate
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
  switch (Statics_Exp.tuple_zip(skel, ty)) {
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
      | InHole(TypeInconsistent, _) => DoesNotElaborate
      | InHole(WrongLength as reason, u) =>
        switch (
          syn_elab_opseq(
            ctx,
            delta,
            opseq |> UHExp.set_err_status_opseq(NotInHole),
          )
        ) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d, _, delta) =>
          let gamma = ctx |> Contexts.gamma;
          let sigma = gamma |> id_env;
          let delta =
            MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
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
    : ElaborationResult.t =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    // tuples handled at opseq level
    DoesNotElaborate
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    ana_elab_operand(ctx, delta, en, ty);
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
      let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1);
      Elaborates(d, Hole, delta);
    };
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(ty_elt) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty_elt', delta) =>
        let d1c = DHExp.cast(d1, ty_elt', ty_elt);
        let ty_list = HTyp.List(ty_elt);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list)) {
        | DoesNotElaborate => DoesNotElaborate
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
      Space,
      _,
      _,
    ) =>
    switch (syn_elab_skel(ctx, delta, skel, seq)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, ty', delta) =>
      if (HTyp.consistent(ty, ty')) {
        Elaborates(d, ty', delta);
      } else {
        DoesNotElaborate;
      }
    }
  }
and ana_elab_operand =
    (ctx: Contexts.t, delta: Delta.t, operand: UHExp.operand, ty: HTyp.t)
    : ElaborationResult.t =>
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
  | If(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _, _)
  | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
    };
  | If(InconsistentBranches(_, u), _, _, _)
  | Case(InconsistentBranches(_, u), _, _) =>
    switch (syn_elab_operand(ctx, delta, operand)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, e_ty, delta) =>
      let gamma = Contexts.gamma(ctx);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
      Elaborates(d, e_ty, delta);
    }
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | If(StandardErrStatus(InHole(WrongLength, _)), _, _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => DoesNotElaborate /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | Var(NotInHole, InVarHole(reason, u), x) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    let d: DHExp.t =
      switch (reason) {
      | Free => FreeVar(u, 0, sigma, x)
      | Keyword(k) => Keyword(u, 0, sigma, k)
      };
    Elaborates(d, ty, delta);
  | Parenthesized(body) => ana_elab(ctx, delta, body, ty)
  | Lam(NotInHole, p, ann, body) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => DoesNotElaborate
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => DoesNotElaborate
        | true =>
          switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1_ann)) {
          | DoesNotElaborate => DoesNotElaborate
          | Elaborates(dp, ty1p, ctx, delta) =>
            switch (ana_elab(ctx, delta, body, ty2)) {
            | DoesNotElaborate => DoesNotElaborate
            | Elaborates(d1, ty2, delta) =>
              let ty = HTyp.Arrow(ty1p, ty2);
              let d = DHExp.Lam(dp, ty1p, d1);
              Elaborates(d, ty, delta);
            }
          }
        };
      | None =>
        switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1_given)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(dp, ty1, ctx, delta) =>
          switch (ana_elab(ctx, delta, body, ty2)) {
          | DoesNotElaborate => DoesNotElaborate
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
    | None => DoesNotElaborate
    | Some((ty1, ty2)) =>
      let e1ty = InjSide.pick(side, ty1, ty2);
      switch (ana_elab(ctx, delta, body, e1ty)) {
      | DoesNotElaborate => DoesNotElaborate
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
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta) =>
      switch (ana_elab_rules(ctx, delta, rules, ty1, ty)) {
      | None => DoesNotElaborate
      | Some((drs, delta)) =>
        let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
        Elaborates(d, ty, delta);
      }
    }
  | If(StandardErrStatus(NotInHole), t1, t2, t3) =>
    switch (syn_elab(ctx, delta, t1)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, _, delta) =>
      switch (syn_elab(ctx, delta, t2)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, _, delta) =>
        switch (syn_elab(ctx, delta, t3)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d3, ty, delta) =>
          let d = DHExp.ConsistentIf(If(d1, d2, d3));
          Elaborates(d, ty, delta);
        }
      }
    }
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(elt_ty) => Elaborates(ListNil(elt_ty), List(elt_ty), delta)
    }
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = id_env(gamma);
    let d = DHExp.InvalidText(u, 0, sigma, t);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | Var(NotInHole, NotInVarHole, _)
  | BoolLit(NotInHole, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | ApPalette(NotInHole, _, _, _) =>
    /* subsumption */
    syn_elab_operand(ctx, delta, operand)
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
  switch (Elaborator_Pat.ana_elab(ctx, delta, p, pat_ty)) {
  | DoesNotElaborate => None
  | Elaborates(dp, _, ctx, delta) =>
    switch (ana_elab(ctx, delta, clause, clause_ty)) {
    | DoesNotElaborate => None
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
  | InvalidText(_)
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
  | ConsistentIf(If(d1, d2, d3)) =>
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    let (d3, hii) = renumber_result_only(path, hii, d3);
    (ConsistentIf(If(d1, d2, d3)), hii);
  | InconsistentBranchesIf(u, _, sigma, If(d1, d2, d3)) =>
    let (i, hii) = HoleInstanceInfo.next(hii, u, sigma, path);
    let (d1, hii) = renumber_result_only(path, hii, d1);
    let (d2, hii) = renumber_result_only(path, hii, d2);
    let (d3, hii) = renumber_result_only(path, hii, d3);
    (InconsistentBranchesIf(u, i, sigma, If(d1, d2, d3)), hii);
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
    (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(DHExp.rule))
    : (list(DHExp.rule), HoleInstanceInfo.t) =>
  rules
  |> List.fold_left(
       (b, r: DHExp.rule) => {
         let (rs, hii) = b;
         switch (r) {
         | Rule(dp, d) =>
           let (dp, hii) =
             Elaborator_Pat.renumber_result_only(path, hii, dp);
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
  | InvalidText(_)
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
  | ConsistentIf(If(d1, d2, d3)) =>
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    let (d3, hii) = renumber_sigmas_only(path, hii, d3);
    (ConsistentIf(If(d1, d2, d3)), hii);
  | InconsistentBranchesIf(u, i, sigma, If(d1, d2, d3)) =>
    let (sigma, hii) = renumber_sigma(path, u, i, hii, sigma);
    let hii = HoleInstanceInfo.update_environment(hii, (u, i), sigma);
    let (d1, hii) = renumber_sigmas_only(path, hii, d1);
    let (d2, hii) = renumber_sigmas_only(path, hii, d2);
    let (d3, hii) = renumber_sigmas_only(path, hii, d3);
    (InconsistentBranchesIf(u, i, sigma, If(d1, d2, d3)), hii);
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
    (path: InstancePath.t, hii: HoleInstanceInfo.t, rules: list(DHExp.rule))
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
