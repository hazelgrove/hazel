type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Contexts.t, Delta.t)
  | LinesDoNotElaborate;

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

let rec syn_elab =
        (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): ElaborationResult.t =>
  syn_elab_block(ctx, delta, e)
and syn_elab_block =
    (ctx: Contexts.t, delta: Delta.t, block: UHExp.block): ElaborationResult.t =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None => DoesNotElaborate
  | Some((leading, conclusion)) =>
    switch (syn_elab_lines(ctx, delta, leading)) {
    | LinesDoNotElaborate => DoesNotElaborate
    | LinesElaborate(prelude, ctx, delta) =>
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
  | [] => LinesElaborate(d => d, ctx, delta)
  | [line, ...lines] =>
    switch (syn_elab_line(ctx, delta, line)) {
    | LinesDoNotElaborate => LinesDoNotElaborate
    | LinesElaborate(prelude_line, ctx, delta) =>
      switch (syn_elab_lines(ctx, delta, lines)) {
      | LinesDoNotElaborate => LinesDoNotElaborate
      | LinesElaborate(prelude_lines, ctx, delta) =>
        LinesElaborate(d => prelude_line(prelude_lines(d)), ctx, delta)
      }
    }
  }
and syn_elab_line =
    (ctx: Contexts.t, delta: Delta.t, line: UHExp.line): elab_result_lines =>
  switch (line) {
  | ExpLine(e1) =>
    switch (syn_elab_opseq(ctx, delta, e1)) {
    | DoesNotElaborate => LinesDoNotElaborate
    | Elaborates(d1, _, delta) =>
      let prelude = d2 => DHExp.Let(Wild, d1, d2);
      LinesElaborate(prelude, ctx, delta);
    }
  | EmptyLine
  | CommentLine(_) => LinesElaborate(d => d, ctx, delta)
  | LetLine(p, def) =>
    switch (Statics_Pat.syn(ctx, p)) {
    | None => LinesDoNotElaborate
    | Some((ty1, _)) =>
      let ctx1 = Statics_Exp.extend_let_def_ctx(ctx, p, def);
      switch (ana_elab(ctx1, delta, def, ty1)) {
      | DoesNotElaborate => LinesDoNotElaborate
      | Elaborates(d1, ty1', delta) =>
        let d1 =
          switch (Statics_Exp.recursive_let_id(ctx, p, def)) {
          | None => d1
          | Some(x) =>
            FixF(
              x,
              ty1',
              Evaluator.subst_var(
                DHExp.cast(BoundVar(x), ty1', ty1),
                x,
                d1,
              ),
            )
          };
        let d1 = DHExp.cast(d1, ty1', ty1);
        switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1)) {
        | DoesNotElaborate => LinesDoNotElaborate
        | Elaborates(dp, _, ctx, delta) =>
          let prelude = d2 => DHExp.Let(dp, d1, d2);
          LinesElaborate(prelude, ctx, delta);
        };
      };
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
      let sigma = Environment.id_env(gamma);
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
  | Fun(InHole(TypeInconsistent as reason, u), _, _)
  | Inj(InHole(TypeInconsistent as reason, u), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta);
    };
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Fun(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _) =>
    DoesNotElaborate
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
        let sigma = Environment.id_env(gamma);
        let delta =
          MetaVarMap.add(u, (Delta.ExpressionHole, HTyp.Hole, gamma), delta);
        let d = DHExp.Case(d1, drs, 0);
        Elaborates(InconsistentBranches(u, 0, sigma, d), Hole, delta);
      };
    } /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let ty = HTyp.Hole;
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
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
    let sigma = Environment.id_env(gamma);
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
  | Fun(NotInHole, p, body, s) =>
    switch (Elaborator_Pat.syn_elab(ctx, delta, p)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, ty1, ctx, delta) =>
      switch (syn_elab(ctx, delta, body)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty2, delta) =>
        let d = DHExp.Fun(dp, ty1, d1, s);
        Elaborates(d, Arrow(ty1, ty2), delta);
      }
    }
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
    | LinesDoNotElaborate => DoesNotElaborate
    | LinesElaborate(prelude, ctx, delta) =>
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
          let sigma = gamma |> Environment.id_env;
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
      let sigma = Environment.id_env(gamma);
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
  | Fun(InHole(TypeInconsistent as reason, u), _, _)
  | Inj(InHole(TypeInconsistent as reason, u), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand')) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta =
        MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), ty, delta);
    };
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
  | Fun(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _) =>
    DoesNotElaborate /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | Var(NotInHole, InVarHole(reason, u), x) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    let d: DHExp.t =
      switch (reason) {
      | Free => FreeVar(u, 0, sigma, x)
      | Keyword(k) => Keyword(u, 0, sigma, k)
      };
    Elaborates(d, ty, delta);
  | Parenthesized(body) => ana_elab(ctx, delta, body, ty)
  | Fun(NotInHole, p, body, s) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => DoesNotElaborate
    | Some((ty1_given, ty2)) =>
      let ty1_ann =
        switch (Statics_Pat.syn(ctx, p)) {
        | None => ty1_given
        | Some((ty_p, _)) => ty_p
        };
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
            let d = DHExp.Fun(dp, ty1p, d1, s);
            Elaborates(d, ty, delta);
          }
        }
      };
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
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(elt_ty) => Elaborates(ListNil(elt_ty), List(elt_ty), delta)
    }
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.InvalidText(u, 0, sigma, t);
    let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    Elaborates(d, ty, delta);
  | Var(NotInHole, NotInVarHole, _)
  | BoolLit(NotInHole, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _) =>
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

/* Bind built-ins before an elaborated expression. */
let elab_wrap_builtins = (d: DHExp.t): DHExp.t =>
  List.fold_left(
    (d', (ident, (_, elab))) => DHExp.Let(Var(ident), elab, d'),
    d,
    Builtins.forms,
  );

let elab = (ctx: Contexts.t, delta: Delta.t, e: UHExp.t): ElaborationResult.t => {
  switch (syn_elab(ctx, delta, e)) {
  | Elaborates(d, ty, delta) =>
    let d' = elab_wrap_builtins(d);
    Elaborates(d', ty, delta);
  | DoesNotElaborate => DoesNotElaborate
  };
};
