type elab_result_lines =
  | LinesElaborate(DHExp.t => DHExp.t, Contexts.t, Delta.t, MetaVarGen.t)
  | LinesDoNotElaborate;

module ElaborationResult = {
  type t =
    | Elaborates(DHExp.t, DHTyp.t, Delta.t, MetaVarGen.t)
    | DoesNotElaborate;

  let to_option =
    fun
    | DoesNotElaborate => None
    | Elaborates(pat, dty, delta, u_gen) => Some((pat, dty, delta, u_gen));

  let from_option =
    fun
    | None => DoesNotElaborate
    | Some((pat, dty, delta, u_gen)) => Elaborates(pat, dty, delta, u_gen);

  let bind = (x: t, ~f: ((DHExp.t, DHTyp.t, Delta.t, MetaVarGen.t)) => t): t =>
    switch (x) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, dty, delta, u_gen) => f((d, dty, delta, u_gen))
    };
};

module Let_syntax = ElaborationResult;

let rec syn_elab =
        (ctx: Contexts.t, delta: Delta.t, e: UHExp.t, u_gen: MetaVarGen.t)
        : ElaborationResult.t =>
  syn_elab_block(ctx, delta, e, u_gen)
and syn_elab_block =
    (ctx: Contexts.t, delta: Delta.t, block: UHExp.block, u_gen: MetaVarGen.t)
    : ElaborationResult.t =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None => DoesNotElaborate
  | Some((leading, conclusion)) =>
    switch (syn_elab_lines(ctx, delta, leading, u_gen)) {
    | LinesDoNotElaborate => DoesNotElaborate
    | LinesElaborate(prelude, ctx, delta, u_gen) =>
      switch (syn_elab_opseq(ctx, delta, conclusion, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d, dty, delta, u_gen) =>
        Elaborates(prelude(d), dty, delta, u_gen)
      }
    }
  }
and syn_elab_lines =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      lines: list(UHExp.line),
      u_gen: MetaVarGen.t,
    )
    : elab_result_lines =>
  switch (lines) {
  | [] => LinesElaborate(d => d, ctx, delta, u_gen)
  | [line, ...lines] =>
    switch (syn_elab_line(ctx, delta, line, u_gen)) {
    | LinesDoNotElaborate => LinesDoNotElaborate
    | LinesElaborate(prelude_line, ctx, delta, u_gen) =>
      switch (syn_elab_lines(ctx, delta, lines, u_gen)) {
      | LinesDoNotElaborate => LinesDoNotElaborate
      | LinesElaborate(prelude_lines, ctx, delta, u_gen) =>
        let pl = d => prelude_line(prelude_lines(d));
        LinesElaborate(pl, ctx, delta, u_gen);
      }
    }
  }
and syn_elab_line =
    (ctx: Contexts.t, delta: Delta.t, line: UHExp.line, u_gen: MetaVarGen.t)
    : elab_result_lines =>
  switch (line) {
  | ExpLine(e1) =>
    switch (syn_elab_opseq(ctx, delta, e1, u_gen)) {
    | DoesNotElaborate => LinesDoNotElaborate
    | Elaborates(d1, _, delta, u_gen) =>
      let prelude = d2 => DHExp.Let(Wild, d1, d2);
      LinesElaborate(prelude, ctx, delta, u_gen);
    }
  | EmptyLine
  | CommentLine(_) => LinesElaborate(d => d, ctx, delta, u_gen)
  | LetLine(p, def) =>
    switch (Statics_Pat.syn(ctx, p)) {
    | None => LinesDoNotElaborate
    | Some((ty1, _)) =>
      let (ctx1, u_gen) = Statics_Exp.extend_let_def_ctx(ctx, p, def, u_gen);
      switch (ana_elab(ctx1, delta, def, ty1, u_gen)) {
      | DoesNotElaborate => LinesDoNotElaborate
      | Elaborates(d1, dty1', delta, u_gen) =>
        let dty1 = DHTyp.lift(Contexts.typing(ctx1), ty1);
        let (d1, u_gen) =
          switch (Statics_Exp.recursive_let_id(ctx, p, def, u_gen)) {
          | None => (d1, u_gen)
          | Some((x, u_gen)) =>
            let d = DHExp.cast(BoundVar(x), dty1', dty1);
            (DHExp.FixF(x, dty1', Evaluator.subst_var(d, x, d1)), u_gen);
          };
        let d1 = DHExp.cast(d1, dty1', dty1);
        switch (Elaborator_Pat.ana_elab(ctx, delta, p, ty1)) {
        | DoesNotElaborate => LinesDoNotElaborate
        | Elaborates(dp, _, ctx, delta) =>
          let prelude = d2 => DHExp.Let(dp, d1, d2);
          LinesElaborate(prelude, ctx, delta, u_gen);
        };
      };
    }
  | TyAliasLine(p, ty) =>
    switch (Elaborator_Typ.syn(ctx, delta, ty)) {
    | None => LinesDoNotElaborate
    | Some((dty, dk, delta)) =>
      let ty = DHTyp.unlift(dty);
      let k = DHTyp.unlift_kind(dk);
      let ctx2 = Statics_TPat.matches(ctx, p, ty, k);
      let prelude = d => DHExp.TyAlias(p, dty, dk, d);
      LinesElaborate(prelude, ctx2, delta, u_gen);
    }
  }
and syn_elab_opseq =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      OpSeq(skel, seq): UHExp.opseq,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  syn_elab_skel(ctx, delta, skel, seq, u_gen)
and syn_elab_skel =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      skel: UHExp.skel,
      seq: UHExp.seq,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    syn_elab_operand(ctx, delta, en, u_gen);
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2)
  | BinOp(InHole(WrongLength as reason, u), Comma as op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta, u_gen) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta =
        MetaVarMap.add(
          u,
          Delta.Hole.Expression(HTyp.Hole(u), gamma),
          delta,
        );
      let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d);
      let dty = DHTyp.lift(Contexts.typing(ctx), Hole(u));
      Elaborates(d, dty, delta, u_gen);
    };
  | BinOp(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (Statics_Exp.syn_skel(ctx, skel1, seq, u_gen)) {
    | None => DoesNotElaborate
    | Some((ty1, u_gen)) =>
      switch (HTyp.matched_arrow(ty1, u_gen)) {
      | None => DoesNotElaborate
      | Some((ty2, ty, u_gen)) =>
        let ty2_arrow_ty = HTyp.Arrow(ty2, ty);
        switch (ana_elab_skel(ctx, delta, skel1, seq, ty2_arrow_ty, u_gen)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d1, dty1', delta, u_gen) =>
          switch (ana_elab_skel(ctx, delta, skel2, seq, ty2, u_gen)) {
          | DoesNotElaborate => DoesNotElaborate
          | Elaborates(d2, dty2', delta, u_gen) =>
            let tyctx = Contexts.typing(ctx);
            let dty2_arrow_ty = DHTyp.lift(tyctx, ty2_arrow_ty);
            let dty2 = DHTyp.lift(tyctx, ty2);
            let dty = DHTyp.lift(tyctx, ty);
            let dc1 = DHExp.cast(d1, dty1', dty2_arrow_ty);
            let dc2 = DHExp.cast(d2, dty2', dty2);
            let d = DHExp.Ap(dc1, dc2);
            Elaborates(d, dty, delta, u_gen);
          }
        };
      }
    }
  | BinOp(NotInHole, Comma, _, _) =>
    switch (UHExp.get_tuple_elements(skel)) {
    | [skel1, skel2, ...tail] =>
      let%bind (d1, dty1, delta, u_gen) =
        syn_elab_skel(ctx, delta, skel1, seq, u_gen);
      let%bind (d2, dty2, delta, u_gen) =
        syn_elab_skel(ctx, delta, skel2, seq, u_gen);
      tail
      |> List.fold_left(
           (acc_opt, skel) => {
             open OptUtil.Syntax;
             let* (pairs, delta, u_gen, dtys) = acc_opt;
             let+ (d, dty, delta, u_gen) =
               syn_elab_skel(ctx, delta, skel, seq, u_gen)
               |> ElaborationResult.to_option;
             (DHExp.Pair(pairs, d), delta, u_gen, dtys @ [dty]);
           },
           Some((DHExp.Pair(d1, d2), delta, u_gen, [dty1, dty2])),
         )
      |> Option.map(((pairs, delta, u_gen, dtys)) => {
           let tyctx = Contexts.typing(ctx);
           let dty = DHTyp.lift(tyctx, Prod(List.map(DHTyp.unlift, dtys)));
           (pairs, dty, delta, u_gen);
         })
      |> ElaborationResult.from_option;
    | _ =>
      raise(
        Invalid_argument(
          "Encountered tuple pattern type with less than 2 elements!",
        ),
      )
    }
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_elab_skel(ctx, delta, skel1, seq, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, dty1, delta, u_gen) =>
      let ty = HTyp.List(DHTyp.unlift(dty1));
      let dty = DHTyp.lift(Contexts.typing(ctx), ty);
      switch (ana_elab_skel(ctx, delta, skel2, seq, ty, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, dty2, delta, u_gen) =>
        let d2c = DHExp.cast(d2, dty2, dty);
        let d = DHExp.Cons(d1, d2c);
        Elaborates(d, dty, delta, u_gen);
      };
    }
  | BinOp(NotInHole, (Plus | Minus | Times | Divide) as op, skel1, skel2)
  | BinOp(NotInHole, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Int, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, dty1, delta, u_gen) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Int, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, dty2, delta, u_gen) =>
        let tyctx = Contexts.typing(ctx);
        let dc1 = DHExp.cast(d1, dty1, DHTyp.lift(tyctx, Int));
        let dc2 = DHExp.cast(d2, dty2, DHTyp.lift(tyctx, Int));
        switch (DHExp.BinIntOp.of_op(op)) {
        | None => DoesNotElaborate
        | Some((op, dty)) =>
          Elaborates(DHExp.BinIntOp(op, dc1, dc2), dty, delta, u_gen)
        };
      }
    }
  | BinOp(NotInHole, (FPlus | FMinus | FTimes | FDivide) as op, skel1, skel2)
  | BinOp(NotInHole, (FLessThan | FGreaterThan | FEquals) as op, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Float, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, dty1, delta, u_gen) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Float, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, dty2, delta, u_gen) =>
        let tyctx = Contexts.typing(ctx);
        let dc1 = DHExp.cast(d1, dty1, DHTyp.lift(tyctx, Float));
        let dc2 = DHExp.cast(d2, dty2, DHTyp.lift(tyctx, Float));
        switch (DHExp.BinFloatOp.of_op(op)) {
        | None => DoesNotElaborate
        | Some((op, dty)) =>
          let d = DHExp.BinFloatOp(op, dc1, dc2);
          Elaborates(d, dty, delta, u_gen);
        };
      }
    }
  | BinOp(NotInHole, (And | Or) as op, skel1, skel2) =>
    switch (ana_elab_skel(ctx, delta, skel1, seq, Bool, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, dty1, delta, u_gen) =>
      switch (ana_elab_skel(ctx, delta, skel2, seq, Bool, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d2, dty2, delta, u_gen) =>
        let tyctx = Contexts.typing(ctx);
        let dc1 = DHExp.cast(d1, dty1, DHTyp.lift(tyctx, Bool));
        let dc2 = DHExp.cast(d2, dty2, DHTyp.lift(tyctx, Bool));
        switch (DHExp.BinBoolOp.of_op(op)) {
        | None => DoesNotElaborate
        | Some(op) =>
          let d = DHExp.BinBoolOp(op, dc1, dc2);
          Elaborates(d, DHTyp.lift(tyctx, Bool), delta, u_gen);
        };
      }
    }
  }
and syn_elab_operand =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      operand: UHExp.operand,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  switch (operand) {
  /* in hole */
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u))
  | Lam(InHole(TypeInconsistent as reason, u), _, _)
  | Inj(InHole(TypeInconsistent as reason, u), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
  | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand', u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta, u_gen) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta =
        MetaVarMap.add(
          u,
          Delta.Hole.Expression(HTyp.Hole(u), gamma),
          delta,
        );
      let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d);
      let dty = DHTyp.lift(Contexts.typing(ctx), Hole(u));
      Elaborates(d, dty, delta, u_gen);
    };
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => DoesNotElaborate
  | Case(InconsistentBranches(rule_types, u), scrut, rules) =>
    switch (syn_elab(ctx, delta, scrut, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, pat_ty, delta, u_gen) =>
      let elab_rules =
        List.fold_left2(
          (b, r_t, r) =>
            switch (b) {
            | None => None
            | Some((drs, delta, u_gen)) =>
              switch (syn_elab_rule(ctx, delta, r, pat_ty, r_t, u_gen)) {
              | None => None
              | Some((dr, delta, u_gen)) =>
                let drs = drs @ [dr];
                Some((drs, delta, u_gen));
              }
            },
          Some(([], delta, u_gen)),
          rule_types,
          rules,
        );
      switch (elab_rules) {
      | None => DoesNotElaborate
      | Some((drs, delta, u_gen)) =>
        let gamma = Contexts.gamma(ctx);
        let sigma = Environment.id_env(gamma);
        let delta =
          MetaVarMap.add(
            u,
            Delta.Hole.Expression(HTyp.Hole(u), gamma),
            delta,
          );
        let d = DHExp.InconsistentBranches(u, 0, sigma, Case(d1, drs, 0));
        let dty = DHTyp.lift(Contexts.typing(ctx), Hole(u));
        Elaborates(d, dty, delta, u_gen);
      };
    } /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let ty = HTyp.Hole(u);
    let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
    Elaborates(d, DHTyp.lift(Contexts.typing(ctx), ty), delta, u_gen);
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.InvalidText(u, 0, sigma, t);
    let ty = HTyp.Hole(u);
    let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
    Elaborates(d, DHTyp.lift(Contexts.typing(ctx), ty), delta, u_gen);
  | Var(NotInHole, NotInVarHole, x) =>
    let gamma = Contexts.gamma(ctx);
    switch (VarMap.lookup(gamma, x)) {
    | Some(ty) =>
      let dty = DHTyp.lift(Contexts.typing(ctx), ty);
      Elaborates(BoundVar(x), dty, delta, u_gen);
    | None => DoesNotElaborate
    };
  | Var(NotInHole, InVarHole(reason, u), x) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let delta =
      MetaVarMap.add(u, Delta.Hole.Expression(HTyp.Hole(u), gamma), delta);
    let d =
      switch (reason) {
      | Free => DHExp.FreeVar(u, 0, sigma, x)
      | Keyword(k) => DHExp.Keyword(u, 0, sigma, k)
      };
    Elaborates(d, DHTyp.lift(Contexts.typing(ctx), Hole(u)), delta, u_gen);
  | IntLit(NotInHole, n) =>
    switch (int_of_string_opt(n)) {
    | Some(n) =>
      let dty = DHTyp.lift(Contexts.typing(ctx), Int);
      Elaborates(IntLit(n), dty, delta, u_gen);
    | None => DoesNotElaborate
    }
  | FloatLit(NotInHole, f) =>
    switch (TextShape.hazel_float_of_string_opt(f)) {
    | Some(f) =>
      let dty = DHTyp.lift(Contexts.typing(ctx), Float);
      Elaborates(FloatLit(f), dty, delta, u_gen);
    | None => DoesNotElaborate
    }
  | BoolLit(NotInHole, b) =>
    let dty = DHTyp.lift(Contexts.typing(ctx), Bool);
    Elaborates(BoolLit(b), dty, delta, u_gen);
  | ListNil(NotInHole) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let ty = HTyp.Hole(u);
    let dty = DHTyp.lift(Contexts.typing(ctx), List(ty));
    let d = DHExp.ListNil(dty);
    Elaborates(d, dty, delta, u_gen);
  | Parenthesized(body) => syn_elab(ctx, delta, body, u_gen)
  | Lam(NotInHole, p, body) =>
    switch (Elaborator_Pat.syn_elab(ctx, delta, p)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(dp, ty1, ctx, delta) =>
      switch (syn_elab(ctx, delta, body, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty2, delta, u_gen) =>
        let d = DHExp.Lam(dp, ty1, d1);
        Elaborates(d, Arrow(ty1, ty2), delta, u_gen);
      }
    }
  | Inj(NotInHole, side, body) =>
    switch (syn_elab(ctx, delta, body, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta, u_gen) =>
      let d = DHExp.Inj(Hole, side, d1);
      let ty =
        switch (side) {
        | L => HTyp.Sum(ty1, Hole)
        | R => HTyp.Sum(Hole, ty1)
        };
      Elaborates(d, ty, delta, u_gen);
    }
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    switch (syn_elab(ctx, delta, scrut, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty, delta, u_gen) =>
      switch (syn_elab_rules(ctx, delta, rules, ty, u_gen)) {
      | None => DoesNotElaborate
      | Some((drs, glb, delta, u_gen)) =>
        let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
        Elaborates(d, glb, delta, u_gen);
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
      u_gen: MetaVarGen.t,
    )
    : option((list(DHExp.rule), HTyp.t, Delta.t, MetaVarGen.t)) =>
  switch (Statics_Exp.syn_rules(ctx, rules, pat_ty, u_gen)) {
  | None => None
  | Some(glb) =>
    let elabed_rule_info =
      List.fold_left(
        (b, r) =>
          switch (b) {
          | None => None
          | Some((drs, delta, u_gen)) =>
            switch (syn_elab_rule(ctx, delta, r, pat_ty, glb, u_gen)) {
            | None => None
            | Some((dr, delta, u_gen)) =>
              let drs = drs @ [dr];
              Some((drs, delta, u_gen));
            }
          },
        Some(([], delta, u_gen)),
        rules,
      );
    switch (elabed_rule_info) {
    | None => None
    | Some((drs, delta, u_gen)) => Some((drs, glb, delta, u_gen))
    };
  }
and syn_elab_rule =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      r: UHExp.rule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : option((DHExp.rule, Delta.t, MetaVarGen.t)) => {
  let UHExp.Rule(p, clause) = r;
  switch (Elaborator_Pat.ana_elab(ctx, delta, p, pat_ty)) {
  | DoesNotElaborate => None
  | Elaborates(dp, _, ctx, delta) =>
    switch (syn_elab(ctx, delta, clause, u_gen)) {
    | DoesNotElaborate => None
    | Elaborates(d1, ty1, delta) =>
      Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta, u_gen))
    }
  };
}
and ana_elab =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      e: UHExp.t,
      ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  ana_elab_block(ctx, delta, e, ty, u_gen)
and ana_elab_block =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      block: UHExp.block,
      ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None => DoesNotElaborate
  | Some((leading, conclusion)) =>
    switch (syn_elab_lines(ctx, delta, leading, u_gen)) {
    | LinesDoNotElaborate => DoesNotElaborate
    | LinesElaborate(prelude, ctx, delta, u_gen) =>
      switch (ana_elab_opseq(ctx, delta, conclusion, ty, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d, ty, delta, u_gen) =>
        Elaborates(prelude(d), ty, delta, u_gen)
      }
    }
  }
and ana_elab_opseq =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t => {
  // handle n-tuples
  switch (Statics_Exp.tuple_zip(skel, ty)) {
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (
           acc:
             option((list(DHExp.t), list(HTyp.t), Delta.t, MetaVarGen.t)),
           (skel: UHExp.skel, ty: HTyp.t, u_gen: MetaVarGen.t),
         ) =>
           switch (acc) {
           | None => None
           | Some((rev_ds, rev_tys, delta, u_gen)) =>
             switch (ana_elab_skel(ctx, delta, skel, seq, ty, u_gen)) {
             | DoesNotElaborate => None
             | Elaborates(d, ty, delta, u_gen) =>
               Some(([d, ...rev_ds], [ty, ...rev_tys], delta, u_gen))
             }
           },
         Some(([], [], delta, u_gen)),
       )
    |> (
      fun
      | None => ElaborationResult.DoesNotElaborate
      | Some((rev_ds, rev_tys, delta, u_gen)) => {
          let d = rev_ds |> List.rev |> DHExp.mk_tuple;
          let ty =
            switch (rev_tys) {
            | [] => failwith("expected at least 1 element")
            | [ty] => ty
            | _ => HTyp.Prod(rev_tys |> List.rev)
            };
          Elaborates(d, ty, delta, u_gen);
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
            MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
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
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    // tuples handled at opseq level
    DoesNotElaborate
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    ana_elab_operand(ctx, delta, en, ty, u_gen);
  | BinOp(InHole(TypeInconsistent as reason, u), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    switch (syn_elab_skel(ctx, delta, skel_not_in_hole, seq, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, _, delta, u_gen) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
      let d = DHExp.NonEmptyHole(reason, u, 0, sigma, d1);
      Elaborates(d, Hole, delta, u_gen);
    };
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(ty_elt) =>
      switch (ana_elab_skel(ctx, delta, skel1, seq, ty_elt, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, ty_elt', delta, u_gen) =>
        let d1c = DHExp.cast(d1, ty_elt', ty_elt);
        let ty_list = HTyp.List(ty_elt);
        switch (ana_elab_skel(ctx, delta, skel2, seq, ty_list, u_gen)) {
        | DoesNotElaborate => DoesNotElaborate
        | Elaborates(d2, ty2, delta, u_gen) =>
          let d2c = DHExp.cast(d2, ty2, ty_list);
          let d = DHExp.Cons(d1c, d2c);
          Elaborates(d, ty_list, delta, u_gen);
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
    switch (syn_elab_skel(ctx, delta, skel, seq, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, ty', delta, u_gen) =>
      if (HTyp.consistent(ty, ty')) {
        Elaborates(d, ty', delta, u_gen);
      } else {
        DoesNotElaborate;
      }
    }
  }
and ana_elab_operand =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      operand: UHExp.operand,
      ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : ElaborationResult.t =>
  switch (operand) {
  /* in hole */
  | Var(InHole(TypeInconsistent as reason, u), _, _)
  | IntLit(InHole(TypeInconsistent as reason, u), _)
  | FloatLit(InHole(TypeInconsistent as reason, u), _)
  | BoolLit(InHole(TypeInconsistent as reason, u), _)
  | ListNil(InHole(TypeInconsistent as reason, u))
  | Lam(InHole(TypeInconsistent as reason, u), _, _)
  | Inj(InHole(TypeInconsistent as reason, u), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent as reason, u)), _, _)
  | ApPalette(InHole(TypeInconsistent as reason, u), _, _, _) =>
    let operand' = operand |> UHExp.set_err_status_operand(NotInHole);
    switch (syn_elab_operand(ctx, delta, operand', u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, _, delta, u_gen) =>
      let gamma = Contexts.gamma(ctx);
      let sigma = Environment.id_env(gamma);
      let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
      Elaborates(NonEmptyHole(reason, u, 0, sigma, d), Hole, delta, u_gen);
    };
  | Case(InconsistentBranches(_, u), _, _) =>
    switch (syn_elab_operand(ctx, delta, operand, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d, e_ty, delta, u_gen) =>
      let gamma = Contexts.gamma(ctx);
      let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
      Elaborates(d, e_ty, delta, u_gen);
    }
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => DoesNotElaborate /* not in hole */
  | EmptyHole(u) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.EmptyHole(u, 0, sigma);
    let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
    Elaborates(d, ty, delta, u_gen);
  | Var(NotInHole, InVarHole(reason, u), x) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
    let d: DHExp.t =
      switch (reason) {
      | Free => FreeVar(u, 0, sigma, x)
      | Keyword(k) => Keyword(u, 0, sigma, k)
      };
    Elaborates(d, ty, delta, u_gen);
  | Parenthesized(body) => ana_elab(ctx, delta, body, ty, u_gen)
  | Lam(NotInHole, p, body) =>
    switch (HTyp.matched_arrow(ty, u_gen)) {
    | None => DoesNotElaborate
    | Some((ty1_given, ty2, u_gen)) =>
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
          switch (ana_elab(ctx, delta, body, ty2, u_gen)) {
          | DoesNotElaborate => DoesNotElaborate
          | Elaborates(d1, ty2, delta, u_gen) =>
            let ty = HTyp.Arrow(ty1p, ty2);
            let d = DHExp.Lam(dp, ty1p, d1);
            Elaborates(d, ty, delta, u_gen);
          }
        }
      };
    }
  | Inj(NotInHole, side, body) =>
    switch (HTyp.matched_sum(ty, u_gen)) {
    | None => DoesNotElaborate
    | Some((ty1, ty2, u_gen)) =>
      let e1ty = InjSide.pick(side, ty1, ty2);
      switch (ana_elab(ctx, delta, body, e1ty, u_gen)) {
      | DoesNotElaborate => DoesNotElaborate
      | Elaborates(d1, e1ty', delta, u_gen) =>
        let (ann_ty, ty) =
          switch (side) {
          | L => (ty2, HTyp.Sum(e1ty', ty2))
          | R => (ty1, HTyp.Sum(ty1, e1ty'))
          };
        let d = DHExp.Inj(ann_ty, side, d1);
        Elaborates(d, ty, delta, u_gen);
      };
    }
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    switch (syn_elab(ctx, delta, scrut, u_gen)) {
    | DoesNotElaborate => DoesNotElaborate
    | Elaborates(d1, ty1, delta, u_gen) =>
      switch (ana_elab_rules(ctx, delta, rules, ty1, ty, u_gen)) {
      | None => DoesNotElaborate
      | Some((drs, delta, u_gen)) =>
        let d = DHExp.ConsistentCase(DHExp.Case(d1, drs, 0));
        Elaborates(d, ty, delta, u_gen, u_gen);
      }
    }
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => DoesNotElaborate
    | Some(elt_ty) =>
      Elaborates(ListNil(elt_ty), List(elt_ty), delta, u_gen)
    }
  | InvalidText(u, t) =>
    let gamma = Contexts.gamma(ctx);
    let sigma = Environment.id_env(gamma);
    let d = DHExp.InvalidText(u, 0, sigma, t);
    let delta = MetaVarMap.add(u, Delta.Hole.Expression(ty, gamma), delta);
    Elaborates(d, ty, delta, u_gen);
  | Var(NotInHole, NotInVarHole, _)
  | BoolLit(NotInHole, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | ApPalette(NotInHole, _, _, _) =>
    /* subsumption */
    syn_elab_operand(ctx, delta, operand, u_gen)
  }
and ana_elab_rules =
    (
      ctx: Contexts.t,
      delta: Delta.t,
      rules: list(UHExp.rule),
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
      u_gen: MetaVarGen.t,
    )
    : option((list(DHExp.rule), Delta.t)) =>
  rules
  |> List.fold_left(
       (b, r) =>
         switch (b) {
         | None => None
         | Some((drs, delta)) =>
           switch (ana_elab_rule(ctx, delta, r, pat_ty, clause_ty, u_gen)) {
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
      u_gen: MetaVarGen.t,
    )
    : option((DHExp.rule, Delta.t, MetaVarGen.t)) => {
  let UHExp.Rule(p, clause) = r;
  switch (Elaborator_Pat.ana_elab(ctx, delta, p, pat_ty)) {
  | DoesNotElaborate => None
  | Elaborates(dp, _, ctx, delta) =>
    switch (ana_elab(ctx, delta, clause, clause_ty, u_gen)) {
    | DoesNotElaborate => None
    | Elaborates(d1, ty1, delta, u_gen) =>
      Some((Rule(dp, DHExp.cast(d1, ty1, clause_ty)), delta, u_gen))
    }
  };
};
