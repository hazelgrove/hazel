let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHExp.get_tuple_elements);

/* returns recursive ctx + name of recursively defined var */
let ctx_for_let' =
    (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, e: UHExp.t)
    : (Contexts.t, option(Var.t)) =>
  switch (p, e) {
  | (
      OpSeq(_, S(Var(_, NotInVarHole, x), E)),
      [ExpLine(OpSeq(_, S(Lam(_), E)))],
    ) =>
    switch (HTyp.matched_arrow(ty)) {
    | Some(_) => (Contexts.extend_gamma(ctx, (x, ty)), Some(x))
    | None => (ctx, None)
    }
  | _ => (ctx, None)
  };

let ctx_for_let =
    (ctx: Contexts.t, p: UHPat.t, ty: HTyp.t, e: UHExp.t): Contexts.t => {
  let (ctx, _) = ctx_for_let'(ctx, p, ty, e);
  ctx;
};

/**
 * Synthesize a type, if possible, for e
 */
let rec syn = (ctx: Contexts.t, e: UHExp.t): option(HTyp.t) =>
  syn_block(ctx, e)
and syn_block = (ctx: Contexts.t, block: UHExp.block): option(HTyp.t) =>
  switch (UHExp.Block.split_conclusion(block)) {
  | None => None
  | Some((leading, conclusion)) =>
    switch (syn_lines(ctx, leading)) {
    | None => None
    | Some(ctx) => syn_opseq(ctx, conclusion)
    }
  }
and syn_lines =
    (ctx: Contexts.t, lines: list(UHExp.line)): option(Contexts.t) => {
  lines
  |> List.fold_left(
       (opt_ctx: option(Contexts.t), line: UHExp.line) =>
         switch (opt_ctx) {
         | None => None
         | Some(ctx) => syn_line(ctx, line)
         },
       Some(ctx),
     );
}
and syn_line = (ctx: Contexts.t, line: UHExp.line): option(Contexts.t) =>
  switch (line) {
  | ExpLine(opseq) => syn_opseq(ctx, opseq) |> OptUtil.map(_ => ctx)
  | EmptyLine
  | CommentLine(_) => Some(ctx)
  | LetLine(p, ann, def) =>
    switch (ann) {
    | Some(uty) =>
      let ty = UHTyp.expand(uty);
      let ctx_def = ctx_for_let(ctx, p, ty, def);
      switch (ana(ctx_def, def, ty)) {
      | None => None
      | Some(_) => Statics_Pat.ana(ctx, p, ty)
      };
    | None =>
      switch (syn(ctx, def)) {
      | None => None
      | Some(ty) => Statics_Pat.ana(ctx, p, ty)
      }
    }
  }
and syn_opseq =
    (ctx: Contexts.t, OpSeq(skel, seq): UHExp.opseq): option(HTyp.t) =>
  syn_skel(ctx, skel, seq)
and syn_skel =
    (ctx: Contexts.t, skel: UHExp.skel, seq: UHExp.seq): option(HTyp.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = Seq.nth_operand(n, seq);
    syn_operand(ctx, en);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    syn_skel(ctx, skel_not_in_hole, seq) |> OptUtil.map(_ => HTyp.Hole);
  | BinOp(NotInHole, Minus | Plus | Times | Divide, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Int)) {
    | None => None
    | Some(_) => ana_skel(ctx, skel2, seq, Int) |> OptUtil.map(_ => HTyp.Int)
    }
  | BinOp(NotInHole, FMinus | FPlus | FTimes | FDivide, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Float)) {
    | None => None
    | Some(_) =>
      ana_skel(ctx, skel2, seq, Float) |> OptUtil.map(_ => HTyp.Float)
    }
  | BinOp(NotInHole, And | Or, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, HTyp.Bool)) {
    | None => None
    | Some(_) =>
      ana_skel(ctx, skel2, seq, HTyp.Bool) |> OptUtil.map(_ => HTyp.Bool)
    }
  | BinOp(NotInHole, LessThan | GreaterThan | Equals, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, Int)) {
    | None => None
    | Some(_) => ana_skel(ctx, skel2, seq, Int) |> OptUtil.map(_ => HTyp.Bool)
    }
  | BinOp(NotInHole, FLessThan | FGreaterThan | FEquals, skel1, skel2) =>
    switch (ana_skel(ctx, skel1, seq, Float)) {
    | None => None
    | Some(_) =>
      ana_skel(ctx, skel2, seq, Float) |> OptUtil.map(_ => HTyp.Bool)
    }
  | BinOp(NotInHole, Space, skel1, skel2) =>
    switch (syn_skel(ctx, skel1, seq)) {
    | None => None
    | Some(ty1) =>
      switch (HTyp.matched_arrow(ty1)) {
      | None => None
      | Some((ty2, ty)) =>
        ana_skel(ctx, skel2, seq, ty2) |> OptUtil.map(_ => ty)
      }
    }
  | BinOp(NotInHole, Comma, _, _) =>
    skel
    |> UHExp.get_tuple_elements
    |> List.map(skel => syn_skel(ctx, skel, seq))
    |> OptUtil.sequence
    |> Option.map(tys => HTyp.Prod(tys))
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (syn_skel(ctx, skel1, seq)) {
    | None => None
    | Some(ty1) =>
      let ty = HTyp.List(ty1);
      ana_skel(ctx, skel2, seq, ty) |> OptUtil.map(_ => ty);
    }
  }
and syn_operand = (ctx: Contexts.t, operand: UHExp.operand): option(HTyp.t) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some(Hole)
  | InvalidText(_) => Some(Hole)
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    syn_operand(ctx, operand') |> OptUtil.map(_ => HTyp.Hole);
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) => None
  | Case(InconsistentBranches(rule_types, _), scrut, rules) =>
    switch (syn(ctx, scrut)) {
    | None => None
    | Some(pat_ty) =>
      /* Make sure the rule synthesizes the type the rule_types says it does */
      let correct_rule_types =
        List.for_all2(
          (rule_ty, rule) => {
            switch (syn_rule(ctx, rule, pat_ty)) {
            | None => false
            | Some(syn_ty) => HTyp.eq(rule_ty, syn_ty)
            }
          },
          rule_types,
          rules,
        );
      if (correct_rule_types) {
        Some(HTyp.Hole);
      } else {
        None;
      };
    }
  /* not in hole */
  | Var(NotInHole, NotInVarHole, x) => VarMap.lookup(Contexts.gamma(ctx), x)
  | Var(NotInHole, InVarHole(_), _) => Some(Hole)
  | IntLit(NotInHole, _) => Some(Int)
  | FloatLit(NotInHole, _) => Some(Float)
  | BoolLit(NotInHole, _) => Some(Bool)
  | ListNil(NotInHole) => Some(List(Hole))
  | Lam(NotInHole, p, ann, body) =>
    let ty1 =
      switch (ann) {
      | Some(uty) => UHTyp.expand(uty)
      | None => HTyp.Hole
      };
    switch (Statics_Pat.ana(ctx, p, ty1)) {
    | None => None
    | Some(ctx) =>
      switch (syn(ctx, body)) {
      | None => None
      | Some(ty2) => Some(HTyp.Arrow(ty1, ty2))
      }
    };
  | Inj(NotInHole, side, body) =>
    switch (syn(ctx, body)) {
    | None => None
    | Some(ty) =>
      switch (side) {
      | L => Some(Sum(ty, Hole))
      | R => Some(Sum(Hole, ty))
      }
    }
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    switch (syn(ctx, scrut)) {
    | None => None
    | Some(b_ty) => syn_rules(ctx, rules, b_ty)
    }
  | ApPalette(NotInHole, name, serialized_model, psi) =>
    let palette_ctx = Contexts.palette_ctx(ctx);
    switch (PaletteCtx.lookup(palette_ctx, name)) {
    | None => None
    | Some(palette_defn) =>
      switch (ana_splice_map(ctx, SpliceInfo.splice_map(psi))) {
      | None => None
      | Some(splice_ctx) =>
        let expansion_ty = palette_defn.expansion_ty;
        let expand = palette_defn.expand;
        let expansion = expand(serialized_model);
        switch (ana(splice_ctx, expansion, expansion_ty)) {
        | None => None
        | Some(_) => Some(expansion_ty)
        };
      }
    };
  | Parenthesized(body) => syn(ctx, body)
  }
and syn_rules =
    (ctx: Contexts.t, rules: UHExp.rules, pat_ty: HTyp.t): option(HTyp.t) => {
  let clause_types =
    List.fold_left(
      (types_opt, r) =>
        switch (types_opt) {
        | None => None
        | Some(types) =>
          switch (syn_rule(ctx, r, pat_ty)) {
          | None => None
          | Some(r_ty) => Some([r_ty, ...types])
          }
        },
      Some([]),
      rules,
    );
  switch (clause_types) {
  | None => None
  | Some(types) => HTyp.join_all(GLB, types)
  };
}
and syn_rule =
    (ctx: Contexts.t, rule: UHExp.rule, pat_ty: HTyp.t): option(HTyp.t) => {
  let Rule(p, clause) = rule;
  switch (Statics_Pat.ana(ctx, p, pat_ty)) {
  | None => None
  | Some(ctx) => syn(ctx, clause)
  };
}
and ana_splice_map =
    (ctx: Contexts.t, splice_map: UHExp.splice_map): option(Contexts.t) =>
  IntMap.fold(
    (splice_name, (ty, e), c) =>
      switch (c) {
      | None => None
      | Some(splice_ctx) =>
        switch (ana(ctx, e, ty)) {
        | None => None
        | Some(_) =>
          let splice_var = SpliceInfo.var_of_splice_name(splice_name);
          Some(Contexts.extend_gamma(splice_ctx, (splice_var, ty)));
        }
      },
    splice_map,
    Some(Contexts.empty),
  )
/**
     * Analyze e against expected type ty
     */
and ana = (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t): option(unit) =>
  ana_block(ctx, e, ty)
and ana_block =
    (ctx: Contexts.t, block: UHExp.block, ty: HTyp.t): option(unit) =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None => None
  | Some((leading, conclusion)) =>
    switch (syn_lines(ctx, leading)) {
    | None => None
    | Some(ctx) => ana_opseq(ctx, conclusion, ty)
    }
  }
and ana_opseq =
    (ctx: Contexts.t, OpSeq(skel, seq) as opseq: UHExp.opseq, ty: HTyp.t)
    : option(unit) =>
  switch (tuple_zip(skel, ty)) {
  | None =>
    switch (UHExp.get_err_status_opseq(opseq), HTyp.get_prod_elements(ty)) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = opseq |> UHExp.set_err_status_opseq(NotInHole);
      syn_opseq(ctx, opseq') |> OptUtil.map(_ => ());
    | _ => None
    }
  | Some(skel_tys) =>
    skel_tys
    |> List.map(((skel, ty)) => ana_skel(ctx, skel, seq, ty))
    |> List.fold_left(OptUtil.map2((_, _) => ()), Some())
  }
and ana_skel =
    (ctx: Contexts.t, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
    : option(unit) =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith(__LOC__ ++ ": tuples handled at opseq level")
  | Placeholder(n) =>
    let en = Seq.nth_operand(n, seq);
    ana_operand(ctx, en, ty);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(ty_elt) =>
      switch (ana_skel(ctx, skel1, seq, ty_elt)) {
      | None => None
      | Some(_) => ana_skel(ctx, skel2, seq, List(ty_elt))
      }
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _)
  | BinOp(
      NotInHole,
      And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
      FDivide |
      LessThan |
      GreaterThan |
      Equals |
      FLessThan |
      FGreaterThan |
      FEquals |
      Space,
      _,
      _,
    ) =>
    switch (syn_skel(ctx, skel, seq)) {
    | None => None
    | Some(ty') => HTyp.consistent(ty, ty') ? Some() : None
    }
  }
and ana_operand =
    (ctx: Contexts.t, operand: UHExp.operand, ty: HTyp.t): option(unit) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some()
  | InvalidText(_) => Some()
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    switch (syn_operand(ctx, operand')) {
    | None => None
    | Some(_) => Some() /* this is a consequence of subsumption and hole universality */
    };
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Lam(InHole(WrongLength, _), _, _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _)
  | ApPalette(InHole(WrongLength, _), _, _, _) =>
    ty |> HTyp.get_prod_elements |> List.length > 1 ? Some() : None
  | Case(InconsistentBranches(_, _), _, _) => None
  /* not in hole */
  | ListNil(NotInHole) =>
    switch (HTyp.matched_list(ty)) {
    | None => None
    | Some(_) => Some()
    }
  | Var(NotInHole, _, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    switch (syn_operand(ctx, operand')) {
    | None => None
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some();
      } else {
        None;
      }
    };
  | Lam(NotInHole, p, ann, body) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => None
    | Some((ty1_given, ty2)) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1_ann = UHTyp.expand(uty1);
        switch (HTyp.consistent(ty1_ann, ty1_given)) {
        | false => None
        | true =>
          switch (Statics_Pat.ana(ctx, p, ty1_ann)) {
          | None => None
          | Some(ctx) => ana(ctx, body, ty2)
          }
        };
      | None =>
        switch (Statics_Pat.ana(ctx, p, ty1_given)) {
        | None => None
        | Some(ctx) => ana(ctx, body, ty2)
        }
      }
    }
  | Inj(NotInHole, side, body) =>
    switch (HTyp.matched_sum(ty)) {
    | None => None
    | Some((ty1, ty2)) => ana(ctx, body, InjSide.pick(side, ty1, ty2))
    }
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    switch (syn(ctx, scrut)) {
    | None => None
    | Some(ty1) => ana_rules(ctx, rules, ty1, ty)
    }
  | ApPalette(NotInHole, _, _, _) =>
    switch (syn_operand(ctx, operand)) {
    | None => None
    | Some(ty') =>
      if (HTyp.consistent(ty, ty')) {
        Some();
      } else {
        None;
      }
    }
  | Parenthesized(body) => ana(ctx, body, ty)
  }
and ana_rules =
    (ctx: Contexts.t, rules: UHExp.rules, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(unit) =>
  List.fold_left(
    (b, r) =>
      switch (b) {
      | None => None
      | Some(_) => ana_rule(ctx, r, pat_ty, clause_ty)
      },
    Some(),
    rules,
  )
and ana_rule =
    (
      ctx: Contexts.t,
      Rule(p, clause): UHExp.rule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : option(unit) =>
  switch (Statics_Pat.ana(ctx, p, pat_ty)) {
  | None => None
  | Some(ctx) => ana(ctx, clause, clause_ty)
  };

/**
     * Get type mode of nth operand of an opseq in synthetic position
     */
let rec syn_nth_type_mode =
        (ctx: Contexts.t, n: int, OpSeq(skel, seq): UHExp.opseq)
        : option(Statics_common.type_mode) =>
  syn_nth_type_mode'(ctx, n, skel, seq)
and syn_nth_type_mode' =
    (ctx: Contexts.t, n: int, skel: UHExp.skel, seq: UHExp.seq)
    : option(Statics_common.type_mode) => {
  let ana_go = (skel, ty) => ana_nth_type_mode'(ctx, n, skel, seq, ty);
  let rec go = (skel: UHExp.skel) =>
    switch (skel) {
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics_common.Syn);
    | BinOp(InHole(_), op, skel1, skel2) =>
      go(BinOp(NotInHole, op, skel1, skel2))
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1) ? go(skel1) : go(skel2)
    | BinOp(NotInHole, Space, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq)) {
      | None => None
      | Some(ty1) =>
        if (n <= Skel.rightmost_tm_index(skel1)) {
          go(skel1);
        } else {
          switch (HTyp.matched_arrow(ty1)) {
          | None => None
          | Some((ty2, _)) => ana_go(skel2, ty2)
          };
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (syn_skel(ctx, skel1, seq)) {
      | None => None
      | Some(ty1) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1) : ana_go(skel2, HTyp.List(ty1))
      }
    | BinOp(
        NotInHole,
        Plus | Minus | Times | Divide | LessThan | GreaterThan,
        skel1,
        skel2,
      ) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, Int) : ana_go(skel2, Int)
    | BinOp(
        NotInHole,
        FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan,
        skel1,
        skel2,
      ) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, Float) : ana_go(skel2, Float)
    | BinOp(NotInHole, And | Or, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, Bool) : ana_go(skel2, Bool)
    | BinOp(NotInHole, Equals, skel1, skel2) =>
      if (n <= Skel.rightmost_tm_index(skel1)) {
        go(skel1);
      } else {
        switch (syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some(ty1) => ana_go(skel2, ty1)
        };
      }
    | BinOp(NotInHole, FEquals, skel1, skel2) =>
      if (n <= Skel.rightmost_tm_index(skel1)) {
        go(skel1);
      } else {
        switch (syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some(ty1) => ana_go(skel2, ty1)
        };
      }
    };
  go(skel);
}
/**
     * Get type mode of nth operand of an opseq in analytic position
     */
and ana_nth_type_mode =
    (
      ctx: Contexts.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      ty: HTyp.t,
    )
    : option(Statics_common.type_mode) => {
  // handle n-tuples
  switch (tuple_zip(skel, ty)) {
  | None =>
    syn_nth_type_mode(ctx, n, opseq |> UHExp.set_err_status_opseq(NotInHole))
  | Some(skel_tys) =>
    let (nskel, nty) =
      skel_tys
      |> List.find(((skel, _)) =>
           Skel.leftmost_tm_index(skel) <= n
           && n <= Skel.rightmost_tm_index(skel)
         );
    ana_nth_type_mode'(ctx, n, nskel, seq, nty);
  };
}
and ana_nth_type_mode' =
    (ctx: Contexts.t, n: int, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
    : option(Statics_common.type_mode) => {
  let syn_go = skel => syn_nth_type_mode'(ctx, n, skel, seq);
  let rec go = (skel: UHExp.skel, ty: HTyp.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics_common.Ana(ty));
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      syn_go(skel_not_in_hole);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        n <= Skel.rightmost_tm_index(skel1)
          ? go(skel1, ty_elt) : go(skel2, ty)
      }
    | BinOp(
        NotInHole,
        And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
        FDivide |
        LessThan |
        GreaterThan |
        Equals |
        FLessThan |
        FGreaterThan |
        FEquals |
        Space,
        _,
        _,
      ) =>
      syn_go(skel)
    };
  go(skel, ty);
};

let stable_syn_fixer = f => {
  let g =
    Statics_common.stable_syn_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input as (), term) =>
      f(ctx, u_gen, ~renumber_empty_holes, term)
    );
  (ctx, u_gen, ~renumber_empty_holes, term) =>
    g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=(), term);
}
and stable_syn_rule_fixer = f => {
  let g =
    Statics_common.stable_syn_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term) => {
      let (fixed_term, extra_output, u_gen, fixed) =
        f(ctx, u_gen, ~renumber_empty_holes, ~pat_ty=extra_input, term);
      (fixed_term, extra_output, u_gen, fixed);
    });
  (ctx, u_gen, ~renumber_empty_holes, ~pat_ty, term) =>
    g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=pat_ty, term);
};

let stable_ana_fixer = f => {
  let g =
    Statics_common.stable_ana_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input as (), term, ty) => {
      let (fixed_term, u_gen, fixed) =
        f(ctx, u_gen, ~renumber_empty_holes, term, ty);
      (fixed_term, (), u_gen, fixed);
    });
  (ctx, u_gen, ~renumber_empty_holes, term, ty) => {
    let (fixed_term, (), u_gen, fixed) =
      g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=(), term, ty);
    (fixed_term, u_gen, fixed);
  };
}
and stable_ana_rule_fixer = f => {
  let g =
    Statics_common.stable_ana_fixer(
      (ctx, u_gen, ~renumber_empty_holes, ~extra_input, term, ty) => {
      let (fixed_term, u_gen, fixed) =
        f(ctx, u_gen, ~renumber_empty_holes, ~pat_ty=extra_input, term, ty);
      (fixed_term, (), u_gen, fixed);
    });
  (ctx, u_gen, ~renumber_empty_holes, ~pat_ty, term, ty) => {
    let (fixed_term, (), u_gen, fixed) =
      g(ctx, u_gen, ~renumber_empty_holes, ~extra_input=pat_ty, term, ty);
    (fixed_term, u_gen, fixed);
  };
};

/* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 */
let rec syn_fix_holes' =
        (
          ctx: Contexts.t,
          u_gen: MetaVarGen.t,
          ~renumber_empty_holes=false,
          e: UHExp.t,
        )
        : (UHExp.t, HTyp.t, MetaVarGen.t, bool) =>
  Lazy.force(syn_fix_holes_block', ctx, u_gen, ~renumber_empty_holes, e)
and syn_fix_holes_block' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        block: UHExp.block,
      ) =>
      switch (UHExp.Block.split_conclusion(block)) {
      | None =>
        let (leading, _ctx, u_gen, _) =
          Lazy.force(
            syn_fix_holes_lines',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            block,
          );
        let (conclusion, u_gen) = UHExp.new_EmptyHole(u_gen);
        (
          leading @ [UHExp.ExpLine(OpSeq.wrap(conclusion))],
          HTyp.Hole,
          u_gen,
          true,
        );
      | Some((leading, conclusion)) =>
        let (leading, ctx, u_gen, lines_fixed) =
          Lazy.force(
            syn_fix_holes_lines',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            leading,
          );
        let (conclusion, ty, u_gen, conclusion_fixed) =
          Lazy.force(
            syn_fix_holes_opseq',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            conclusion,
          );
        (
          leading @ [UHExp.ExpLine(conclusion)],
          ty,
          u_gen,
          lines_fixed || conclusion_fixed,
        );
      }
    )
  )
and syn_fix_holes_lines' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        lines: list(UHExp.line),
      ) => {
      let (rev_lines, ctx, u_gen, fixed) =
        lines
        |> List.fold_left(
             (
               (lines, ctx, u_gen, fixed): (
                 list(UHExp.line),
                 Contexts.t,
                 MetaVarGen.t,
                 bool,
               ),
               line: UHExp.line,
             ) => {
               let (line, ctx, u_gen, line_fixed) =
                 Lazy.force(
                   syn_fix_holes_line',
                   ctx,
                   u_gen,
                   ~renumber_empty_holes,
                   line,
                 );
               ([line, ...lines], ctx, u_gen, fixed || line_fixed);
             },
             ([], ctx, u_gen, false),
           );
      (List.rev(rev_lines), ctx, u_gen, fixed);
    })
  )
and syn_fix_holes_line' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        line: UHExp.line,
      ) =>
      switch (line) {
      | ExpLine(e) =>
        let (e, _, u_gen, fixed) =
          Lazy.force(
            syn_fix_holes_opseq',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            e,
          );
        (ExpLine(e), ctx, u_gen, fixed);
      | EmptyLine
      | CommentLine(_) => (line, ctx, u_gen, false)
      | LetLine(p, ann, def) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1 = UHTyp.expand(uty1);
          let ctx_def = ctx_for_let(ctx, p, ty1, def);
          let (def, u_gen, def_fixed) =
            ana_fix_holes'(ctx_def, u_gen, ~renumber_empty_holes, def, ty1);
          let (p, ctx, u_gen, pat_fixed) =
            Statics_Pat.ana_fix_holes'(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              p,
              ty1,
            );
          (LetLine(p, ann, def), ctx, u_gen, pat_fixed || def_fixed);
        | None =>
          let (def, ty1, u_gen, def_fixed) =
            syn_fix_holes'(~renumber_empty_holes, ctx, u_gen, def);
          let (p, ctx, u_gen, pat_fixed) =
            Statics_Pat.ana_fix_holes'(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              p,
              ty1,
            );
          (LetLine(p, ann, def), ctx, u_gen, pat_fixed || def_fixed);
        }
      }
    )
  )
and syn_fix_holes_opseq' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        OpSeq(skel, seq): UHExp.opseq,
      ) => {
      let ((skel, seq), ty, u_gen, fixed) =
        Lazy.force(
          syn_fix_holes_skel',
          ctx,
          u_gen,
          ~renumber_empty_holes,
          (skel, seq),
        );
      (OpSeq(skel, seq), ty, u_gen, fixed);
    })
  )
and syn_fix_holes_skel' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        (skel: UHExp.skel, seq: UHExp.seq),
      ) =>
      (
        switch (skel) {
        | Placeholder(n) =>
          let en = seq |> Seq.nth_operand(n);
          let (en, ty, u_gen, fixed) =
            Lazy.force(
              syn_fix_holes_operand',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              en,
            );
          let seq = seq |> Seq.update_nth_operand(n, en);
          ((skel, seq), ty, u_gen, fixed);
        | BinOp(err, (Minus | Plus | Times | Divide) as op, skel1, skel2) =>
          let ((skel1, seq), u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Int,
            );
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Int,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          ((BinOp(NotInHole, op, skel1, skel2), seq), Int, u_gen, fixed);
        | BinOp(err, (FMinus | FPlus | FTimes | FDivide) as op, skel1, skel2) =>
          let ((skel1, seq), u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Float,
            );
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Float,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          ((BinOp(NotInHole, op, skel1, skel2), seq), Float, u_gen, fixed);
        | BinOp(err, (And | Or) as op, skel1, skel2) =>
          let ((skel1, seq), u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Bool,
            );
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Bool,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          ((BinOp(NotInHole, op, skel1, skel2), seq), Bool, u_gen, fixed);
        | BinOp(err, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
          let ((skel1, seq), u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Int,
            );
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Int,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          ((BinOp(NotInHole, op, skel1, skel2), seq), Bool, u_gen, fixed);
        | BinOp(err, (FLessThan | FGreaterThan | FEquals) as op, skel1, skel2) =>
          let ((skel1, seq), u_gen, fixed1) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
              HTyp.Float,
            );
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              HTyp.Float,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          ((BinOp(NotInHole, op, skel1, skel2), seq), Bool, u_gen, fixed);
        | BinOp(err, Space, skel1, skel2) =>
          let ((skel1, seq), ty1, u_gen, fixed1) =
            Lazy.force(
              syn_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
            );
          switch (HTyp.matched_arrow(ty1)) {
          | Some((ty2, ty)) =>
            let ((skel2, seq), u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                ty2,
              );
            let fixed = fixed1 || fixed2 || err != NotInHole;
            (
              (BinOp(NotInHole, Space, skel1, skel2), seq),
              ty,
              u_gen,
              fixed,
            );
          | None =>
            let ((skel2, seq), u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                HTyp.Hole,
              );
            let (err1, u_gen, fixed1) = {
              let inner_fixed1 = fixed1;
              let err1 = UHExp.get_err_status_opseq(OpSeq(skel1, seq));
              let (err1, u_gen, outer_fixed1) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err1);
              (err1, u_gen, inner_fixed1 || outer_fixed1);
            };
            let OpSeq(skel1, seq) =
              UHExp.set_err_status_opseq(err1, OpSeq(skel1, seq));
            let fixed = fixed1 || fixed2 || err != NotInHole;
            (
              (BinOp(NotInHole, Space, skel1, skel2), seq),
              Hole,
              u_gen,
              fixed,
            );
          };
        | BinOp(err, Comma, _, _) =>
          let ((u_gen, seq, fixed), pairs) =
            skel
            |> UHExp.get_tuple_elements
            |> ListUtil.map_with_accumulator(
                 ((u_gen, seq, fixed), skel) => {
                   let ((skel, seq), ty, u_gen, elem_fixed) =
                     Lazy.force(
                       syn_fix_holes_skel',
                       ctx,
                       u_gen,
                       ~renumber_empty_holes,
                       (skel, seq),
                     );
                   ((u_gen, seq, fixed || elem_fixed), (skel, ty));
                 },
                 (u_gen, seq, false),
               );
          let (skels, tys) = List.split(pairs);
          (
            (UHExp.mk_tuple(skels), seq),
            Prod(tys),
            u_gen,
            fixed || err != NotInHole,
          );
        | BinOp(err, Cons, skel1, skel2) =>
          let ((skel1, seq), ty_elt, u_gen, fixed1) =
            Lazy.force(
              syn_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel1, seq),
            );
          let ty = HTyp.List(ty_elt);
          let ((skel2, seq), u_gen, fixed2) =
            Lazy.force(
              ana_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel2, seq),
              ty,
            );
          let fixed = fixed1 || fixed2 || err != NotInHole;
          let skel = Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
          ((skel, seq), ty, u_gen, fixed);
        }: (
          (UHExp.skel, UHExp.seq),
          HTyp.t,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and syn_fix_holes_operand' =
  lazy(
    stable_syn_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        operand: UHExp.operand,
      ) => {
      let was_in_hole = UHExp.get_err_status_operand(operand) != NotInHole;
      let operand_nih = UHExp.set_err_status_operand(NotInHole, operand);
      switch (operand) {
      | EmptyHole(_) =>
        if (renumber_empty_holes) {
          let (u, u_gen) = MetaVarGen.next(u_gen);
          (EmptyHole(u), HTyp.Hole, u_gen, true);
        } else {
          (operand, Hole, u_gen, false);
        }
      | Var(_, var_err_status, x) =>
        let gamma = Contexts.gamma(ctx);
        switch (VarMap.lookup(gamma, x)) {
        | Some(ty) => (
            UHExp.Var(NotInHole, NotInVarHole, x),
            ty,
            u_gen,
            was_in_hole,
          )
        | None =>
          switch (var_err_status) {
          | InVarHole(_) => (operand_nih, HTyp.Hole, u_gen, false)
          | NotInVarHole =>
            let (u, u_gen) = MetaVarGen.next(u_gen);
            let reason: VarErrStatus.HoleReason.t =
              switch (Var.is_let(x), Var.is_case(x)) {
              | (true, _) => Keyword(Let)
              | (_, true) => Keyword(Case)
              | _ => Free
              };
            (Var(NotInHole, InVarHole(reason, u), x), Hole, u_gen, true);
          }
        };
      | InvalidText(_) => (operand_nih, Hole, u_gen, false)
      | IntLit(_) => (operand_nih, Int, u_gen, was_in_hole)
      | FloatLit(_) => (operand_nih, Float, u_gen, was_in_hole)
      | BoolLit(_) => (operand_nih, Bool, u_gen, was_in_hole)
      | ListNil(_) => (operand_nih, List(Hole), u_gen, was_in_hole)
      | Parenthesized(body) =>
        let (block, ty, u_gen, fixed) =
          syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, body);
        (Parenthesized(block), ty, u_gen, fixed);
      | Lam(_, p, ann, body) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => HTyp.Hole
          };
        let (p, ctx, u_gen, p_fixed) =
          Statics_Pat.ana_fix_holes'(
            ctx,
            u_gen,
            ~renumber_empty_holes,
            p,
            ty1,
          );
        let (body, ty2, u_gen, body_fixed) =
          syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, body);
        (
          Lam(NotInHole, p, ann, body),
          Arrow(ty1, ty2),
          u_gen,
          p_fixed || body_fixed || was_in_hole,
        );
      | Inj(_, side, body) =>
        let (body, ty1, u_gen, body_fixed) =
          syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, body);
        let ty =
          switch (side) {
          | L => HTyp.Sum(ty1, Hole)
          | R => HTyp.Sum(Hole, ty1)
          };
        (Inj(NotInHole, side, body), ty, u_gen, body_fixed || was_in_hole);
      | Case(case_err, scrut, rules) =>
        let (scrut, ty1, u_gen, scrut_fixed) =
          syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, scrut);
        let (rules, (rule_types, common_type), u_gen, fixed_rules) =
          Lazy.force(
            syn_fix_holes_rules',
            ctx,
            u_gen,
            ~renumber_empty_holes,
            ~pat_ty=ty1,
            rules,
          );
        switch (common_type) {
        | None =>
          let (case_err, u_gen, fixed) =
            Statics_common.set_inconsistent_branches(
              u_gen,
              rule_types,
              case_err,
            );
          (
            Case(case_err, scrut, rules),
            HTyp.Hole,
            u_gen,
            scrut_fixed || fixed_rules || fixed,
          );
        | Some(common_type) => (
            Case(StandardErrStatus(NotInHole), scrut, rules),
            common_type,
            u_gen,
            scrut_fixed || fixed_rules || was_in_hole,
          )
        };
      | ApPalette(_, name, serialized_model, psi) =>
        let palette_ctx = Contexts.palette_ctx(ctx);
        switch (PaletteCtx.lookup(palette_ctx, name)) {
        | None => raise(PaletteCtx.InvalidPaletteHoleName) /* TODO invalid palette name hole */
        | Some(palette_defn) =>
          let (splice_map, u_gen, splices_fixed) =
            ana_fix_holes_splice_map'(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              SpliceInfo.splice_map(psi),
            );
          let psi = SpliceInfo.update_splice_map(psi, splice_map);
          let expansion_ty = palette_defn.expansion_ty;
          (
            ApPalette(NotInHole, name, serialized_model, psi),
            expansion_ty,
            u_gen,
            splices_fixed || was_in_hole,
          );
        };
      };
    })
  )
and syn_fix_holes_rules' =
  lazy(
    stable_syn_rule_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        ~pat_ty: HTyp.t,
        rules: UHExp.rules,
      ) => {
      let (rev_fixed_rules, u_gen, rule_types, fixed) =
        List.fold_left(
          ((rules, u_gen, rule_types, fixed), r) => {
            let (r, r_ty, u_gen, rule_fixed) =
              Lazy.force(
                syn_fix_holes_rule',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                ~pat_ty,
                r,
              );
            (
              [r, ...rules],
              u_gen,
              [r_ty, ...rule_types],
              fixed || rule_fixed,
            );
          },
          ([], u_gen, [], false),
          rules,
        );
      let common_type = HTyp.join_all(GLB, rule_types);
      (
        List.rev(rev_fixed_rules),
        (List.rev(rule_types), common_type),
        u_gen,
        fixed,
      );
    })
  )
and syn_fix_holes_rule' =
  lazy(
    stable_syn_rule_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        ~pat_ty: HTyp.t,
        rule: UHExp.rule,
      ) => {
      let Rule(p, clause) = rule;
      let (p, ctx, u_gen, p_fixed) =
        Statics_Pat.ana_fix_holes'(
          ctx,
          u_gen,
          ~renumber_empty_holes,
          p,
          pat_ty,
        );
      let (clause, clause_ty, u_gen, clause_fixed) =
        syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, clause);
      (Rule(p, clause), clause_ty, u_gen, p_fixed || clause_fixed);
    })
  )
and ana_fix_holes_rules' =
  lazy(
    stable_ana_rule_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        ~pat_ty: HTyp.t,
        rules: UHExp.rules,
        clause_ty: HTyp.t,
      ) =>
      (
        {
          let (rev_fixed_rules, u_gen, fixed) =
            List.fold_left(
              ((rules, u_gen, fixed), r) => {
                let (r, u_gen, rule_fixed) =
                  Lazy.force(
                    ana_fix_holes_rule',
                    ctx,
                    u_gen,
                    ~renumber_empty_holes,
                    ~pat_ty,
                    r,
                    clause_ty,
                  );
                ([r, ...rules], u_gen, fixed || rule_fixed);
              },
              ([], u_gen, false),
              rules,
            );
          (List.rev(rev_fixed_rules), u_gen, fixed);
        }: (
          UHExp.rules,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_rule' =
  lazy(
    stable_ana_rule_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes: bool,
        ~pat_ty: HTyp.t,
        Rule(p, clause): UHExp.rule,
        clause_ty: HTyp.t,
      ) =>
      (
        {
          let (p, ctx, u_gen, p_fixed) =
            Statics_Pat.ana_fix_holes'(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              p,
              pat_ty,
            );
          let (clause, u_gen, clause_fixed) =
            ana_fix_holes'(
              ctx,
              u_gen,
              ~renumber_empty_holes,
              clause,
              clause_ty,
            );
          (Rule(p, clause), u_gen, p_fixed || clause_fixed);
        }: (
          UHExp.rule,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_splice_map' =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      splice_map: UHExp.splice_map,
    )
    : (UHExp.splice_map, MetaVarGen.t, bool) =>
  IntMap.fold(
    (splice_name, (ty, e), (splice_map, u_gen, fixed)) => {
      let (e, u_gen, splice_fixed) =
        ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, e, ty);
      let splice_map = splice_map |> IntMap.add(splice_name, (ty, e));
      (splice_map, u_gen, fixed || splice_fixed);
    },
    splice_map,
    (splice_map, u_gen, false),
  )
and ana_fix_holes' =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.t,
      ty: HTyp.t,
    )
    : (UHExp.t, MetaVarGen.t, bool) =>
  Lazy.force(ana_fix_holes_block', ctx, u_gen, ~renumber_empty_holes, e, ty)
and ana_fix_holes_block' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        block: UHExp.block,
        ty: HTyp.t,
      ) =>
      (
        switch (UHExp.Block.split_conclusion(block)) {
        | None =>
          let (leading, _ctx, u_gen, _) =
            Lazy.force(
              syn_fix_holes_lines',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              block,
            );
          let (conclusion, u_gen) = UHExp.new_EmptyHole(u_gen);
          (leading @ [UHExp.ExpLine(OpSeq.wrap(conclusion))], u_gen, true);
        | Some((leading, conclusion)) =>
          let (leading, ctx, u_gen, fixed_leading) =
            Lazy.force(
              syn_fix_holes_lines',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              leading,
            );
          let (conclusion, u_gen, fixed_conclusion) =
            Lazy.force(
              ana_fix_holes_opseq',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              conclusion,
              ty,
            );
          (
            leading @ [UHExp.ExpLine(conclusion)],
            u_gen,
            fixed_leading || fixed_conclusion,
          );
        }: (
          UHExp.block,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_opseq' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        OpSeq(skel, seq) as opseq: UHExp.opseq,
        ty: HTyp.t,
      ) =>
      (
        // handle n-tuples
        {
          let err = UHExp.get_err_status_opseq(opseq);
          switch (tuple_zip(skel, ty)) {
          | Some(skel_tys) =>
            skel_tys
            |> List.fold_left(
                 (
                   (
                     rev_skels: list(UHExp.skel),
                     seq: UHExp.seq,
                     u_gen: MetaVarGen.t,
                     fixed: bool,
                   ),
                   (skel: UHExp.skel, ty: HTyp.t),
                 ) => {
                   let ((skel, seq), u_gen, elem_fixed) =
                     Lazy.force(
                       ana_fix_holes_skel',
                       ctx,
                       u_gen,
                       ~renumber_empty_holes,
                       (skel, seq),
                       ty,
                     );
                   ([skel, ...rev_skels], seq, u_gen, fixed || elem_fixed);
                 },
                 ([], seq, u_gen, false),
               )
            |> (
              fun
              | (rev_skels, seq, u_gen, fixed_elems) => {
                  let fixed = err != NotInHole || fixed_elems;
                  let skel = UHExp.mk_tuple(List.rev(rev_skels));
                  (OpSeq.OpSeq(skel, seq), u_gen, fixed);
                }
            )
          | None =>
            if (List.length(HTyp.get_prod_elements(ty)) == 1) {
              skel
              |> UHExp.get_tuple_elements
              |> List.fold_left(
                   (
                     (
                       rev_skels: list(UHExp.skel),
                       seq: UHExp.seq,
                       u_gen: MetaVarGen.t,
                       fixed: bool,
                     ),
                     skel: UHExp.skel,
                   ) => {
                     let ((skel, seq), _, u_gen, elem_fixed) =
                       Lazy.force(
                         syn_fix_holes_skel',
                         ctx,
                         u_gen,
                         ~renumber_empty_holes,
                         (skel, seq),
                       );
                     ([skel, ...rev_skels], seq, u_gen, fixed || elem_fixed);
                   },
                   ([], seq, u_gen, false),
                 )
              |> (
                fun
                | (rev_skels, seq, u_gen, fixed_elems) => {
                    let (err, u_gen, fixed) =
                      Statics_common.set_hole_reason(
                        u_gen,
                        TypeInconsistent,
                        err,
                      );
                    let skel = UHExp.mk_tuple(List.rev(rev_skels));
                    let opseq =
                      UHExp.set_err_status_opseq(
                        err,
                        OpSeq.OpSeq(skel, seq),
                      );
                    (opseq, u_gen, fixed || fixed_elems);
                  }
              );
            } else {
              let (err, u_gen, fixed) =
                Statics_common.set_hole_reason(u_gen, WrongLength, err);
              let (opseq, _, u_gen, fixed_elems) =
                Lazy.force(
                  syn_fix_holes_opseq',
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  UHExp.set_err_status_opseq(NotInHole, opseq),
                );
              (
                UHExp.set_err_status_opseq(err, opseq),
                u_gen,
                fixed || fixed_elems,
              );
            }
          };
        }: (
          UHExp.opseq,
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_skel' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes,
        (skel: UHExp.skel, seq: UHExp.seq),
        ty: HTyp.t,
      ) =>
      (
        switch (skel) {
        | BinOp(_, Comma, _, _) =>
          failwith("Exp.ana_fix_holes_skel: tuples handled at opseq level")
        | Placeholder(n) =>
          let en = seq |> Seq.nth_operand(n);
          let (en, u_gen, fixed) =
            Lazy.force(
              ana_fix_holes_operand',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              en,
              ty,
            );
          let seq = seq |> Seq.update_nth_operand(n, en);
          ((skel, seq), u_gen, fixed);
        | BinOp(err, Cons, skel1, skel2) =>
          switch (HTyp.matched_list(ty)) {
          | Some(ty_elt) =>
            let ((skel1, seq), u_gen, fixed1) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel1, seq),
                ty_elt,
              );
            let ty_list = HTyp.List(ty_elt);
            let ((skel2, seq), u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                ty_list,
              );
            let skel =
              Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
            ((skel, seq), u_gen, err != NotInHole || fixed1 || fixed2);
          | None =>
            let ((skel1, seq), ty_elt, u_gen, fixed1) =
              Lazy.force(
                syn_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel1, seq),
              );
            let ty_list = HTyp.List(ty_elt);
            let ((skel2, seq), u_gen, fixed2) =
              Lazy.force(
                ana_fix_holes_skel',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                (skel2, seq),
                ty_list,
              );
            let (err, u_gen, outer_fixed) =
              Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
            let skel = Skel.BinOp(err, Operators_Exp.Cons, skel1, skel2);
            ((skel, seq), u_gen, outer_fixed || fixed1 || fixed2);
          }
        | BinOp(
            err,
            And | Or | Minus | Plus | Times | Divide | FMinus | FPlus | FTimes |
            FDivide |
            LessThan |
            GreaterThan |
            Equals |
            FLessThan |
            FGreaterThan |
            FEquals |
            Space,
            _,
            _,
          ) =>
          let ((skel, seq), ty', u_gen, inner_fixed) =
            Lazy.force(
              syn_fix_holes_skel',
              ctx,
              u_gen,
              ~renumber_empty_holes,
              (skel, seq),
            );
          if (HTyp.consistent(ty, ty')) {
            ((skel, seq), u_gen, inner_fixed || err != NotInHole);
          } else {
            let (err, u_gen, outer_fixed) =
              Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
            let OpSeq(skel, seq) =
              UHExp.set_err_status_opseq(err, OpSeq(skel, seq));
            ((skel, seq), u_gen, outer_fixed || inner_fixed);
          };
        }: (
          (UHExp.skel, UHExp.seq),
          MetaVarGen.t,
          bool,
        )
      )
    )
  )
and ana_fix_holes_operand' =
  lazy(
    stable_ana_fixer(
      (
        ctx: Contexts.t,
        u_gen: MetaVarGen.t,
        ~renumber_empty_holes: bool,
        operand: UHExp.operand,
        ty: HTyp.t,
      ) =>
      (
        {
          let err = UHExp.get_err_status_operand(operand);
          let was_in_hole = err != NotInHole;
          let operand_nih = UHExp.set_err_status_operand(NotInHole, operand);
          switch (operand) {
          | EmptyHole(_) =>
            if (renumber_empty_holes) {
              let (u, u_gen) = MetaVarGen.next(u_gen);
              (EmptyHole(u), u_gen, true);
            } else {
              (operand, u_gen, false);
            }
          | InvalidText(_) => (operand_nih, u_gen, false)
          | Var(_)
          | IntLit(_)
          | FloatLit(_)
          | BoolLit(_) =>
            let (operand, ty', u_gen, inner_fixed) =
              Lazy.force(
                syn_fix_holes_operand',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                operand_nih,
              );
            if (HTyp.consistent(ty, ty')) {
              (
                UHExp.set_err_status_operand(NotInHole, operand),
                u_gen,
                was_in_hole || inner_fixed,
              );
            } else {
              let (err, u_gen, outer_fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (
                UHExp.set_err_status_operand(err, operand),
                u_gen,
                outer_fixed || inner_fixed,
              );
            };
          | ListNil(_) =>
            switch (HTyp.matched_list(ty)) {
            | Some(_) => (
                UHExp.set_err_status_operand(NotInHole, operand),
                u_gen,
                was_in_hole,
              )
            | None =>
              let (err, u_gen, fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (ListNil(err), u_gen, fixed);
            }
          | Parenthesized(body) =>
            let (body, u_gen, body_fixed) =
              ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, body, ty);
            (Parenthesized(body), u_gen, body_fixed);
          | Lam(_, p, ann, def) =>
            switch (HTyp.matched_arrow(ty)) {
            | Some((ty1_given, ty2)) =>
              switch (ann) {
              | Some(uty1) =>
                let ty1_ann = UHTyp.expand(uty1);
                if (HTyp.consistent(ty1_ann, ty1_given)) {
                  let (p, ctx, u_gen, p_fixed) =
                    Statics_Pat.ana_fix_holes'(
                      ctx,
                      u_gen,
                      ~renumber_empty_holes,
                      p,
                      ty1_ann,
                    );
                  let (def, u_gen, def_fixed) =
                    ana_fix_holes'(
                      ctx,
                      u_gen,
                      ~renumber_empty_holes,
                      def,
                      ty2,
                    );
                  (
                    UHExp.Lam(NotInHole, p, ann, def),
                    u_gen,
                    was_in_hole || p_fixed || def_fixed,
                  );
                } else {
                  let (operand', _, u_gen, inner_fixed) =
                    Lazy.force(
                      syn_fix_holes_operand',
                      ctx,
                      u_gen,
                      ~renumber_empty_holes,
                      operand_nih,
                    );
                  let (err, u_gen, outer_fixed) =
                    Statics_common.set_hole_reason(
                      u_gen,
                      TypeInconsistent,
                      err,
                    );
                  (
                    UHExp.set_err_status_operand(err, operand'),
                    u_gen,
                    outer_fixed || inner_fixed,
                  );
                };
              | None =>
                let (p, ctx, u_gen, p_fixed) =
                  Statics_Pat.ana_fix_holes'(
                    ctx,
                    u_gen,
                    ~renumber_empty_holes,
                    p,
                    ty1_given,
                  );
                let (def, u_gen, def_fixed) =
                  ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, def, ty2);
                (
                  UHExp.Lam(NotInHole, p, ann, def),
                  u_gen,
                  was_in_hole || p_fixed || def_fixed,
                );
              }
            | None =>
              let (operand', _, u_gen, inner_fixed) =
                Lazy.force(
                  syn_fix_holes_operand',
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  operand_nih,
                );
              let (err, u_gen, outer_fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (
                UHExp.set_err_status_operand(err, operand'),
                u_gen,
                outer_fixed || inner_fixed,
              );
            }
          | Inj(_, side, body) =>
            switch (HTyp.matched_sum(ty)) {
            | Some((ty1, ty2)) =>
              let (e1, u_gen, body_fixed) =
                ana_fix_holes'(
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  body,
                  InjSide.pick(side, ty1, ty2),
                );
              (Inj(NotInHole, side, e1), u_gen, was_in_hole || body_fixed);
            | None =>
              let (operand', ty', u_gen, inner_fixed) =
                Lazy.force(
                  syn_fix_holes_operand',
                  ctx,
                  u_gen,
                  ~renumber_empty_holes,
                  operand_nih,
                );
              if (HTyp.consistent(ty, ty')) {
                (
                  UHExp.set_err_status_operand(NotInHole, operand'),
                  u_gen,
                  was_in_hole || inner_fixed,
                );
              } else {
                let (err, u_gen, outer_fixed) =
                  Statics_common.set_hole_reason(
                    u_gen,
                    TypeInconsistent,
                    err,
                  );
                (
                  UHExp.set_err_status_operand(err, operand'),
                  u_gen,
                  outer_fixed || inner_fixed,
                );
              };
            }
          | Case(case_err, scrut, rules) =>
            let was_in_hole = case_err != StandardErrStatus(NotInHole);
            let (scrut, scrut_ty, u_gen, scrut_fixed) =
              syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, scrut);
            let (rules, u_gen, fixed_rules) =
              Lazy.force(
                ana_fix_holes_rules',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                ~pat_ty=scrut_ty,
                rules,
                ty,
              );
            (
              Case(StandardErrStatus(NotInHole), scrut, rules),
              u_gen,
              was_in_hole || scrut_fixed || fixed_rules,
            );
          | ApPalette(_, _, _, _) =>
            let (operand', ty', u_gen, inner_fixed) =
              Lazy.force(
                syn_fix_holes_operand',
                ctx,
                u_gen,
                ~renumber_empty_holes,
                operand_nih,
              );
            if (HTyp.consistent(ty, ty')) {
              (
                UHExp.set_err_status_operand(NotInHole, operand'),
                u_gen,
                was_in_hole || inner_fixed,
              );
            } else {
              let (err, u_gen, outer_fixed) =
                Statics_common.set_hole_reason(u_gen, TypeInconsistent, err);
              (
                UHExp.set_err_status_operand(err, operand'),
                u_gen,
                outer_fixed || inner_fixed,
              );
            };
          };
        }: (
          UHExp.operand,
          MetaVarGen.t,
          bool,
        )
      )
    )
  );

let syn_fix_holes = (ctx, u_gen, ~renumber_empty_holes=false, e) => {
  let (e, ty, u_gen, _) =
    syn_fix_holes'(ctx, u_gen, ~renumber_empty_holes, e);
  (e, ty, u_gen);
};
let syn_fix_holes_block = (ctx, u_gen, ~renumber_empty_holes=false, block) => {
  let (block, ty, u_gen, _) =
    Lazy.force(
      syn_fix_holes_block',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      block,
    );
  (block, ty, u_gen);
};
let syn_fix_holes_opseq = (ctx, u_gen, ~renumber_empty_holes=false, opseq) => {
  let (opseq, ty, u_gen, _) =
    Lazy.force(
      syn_fix_holes_opseq',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      opseq,
    );
  (opseq, ty, u_gen);
};
let syn_fix_holes_rules =
    (ctx, u_gen, ~renumber_empty_holes=false, rules, pat_ty) => {
  let (rules, (rule_types, common_type), u_gen, _) =
    Lazy.force(
      syn_fix_holes_rules',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      ~pat_ty,
      rules,
    );
  (rules, u_gen, rule_types, common_type);
};
let ana_fix_holes = (ctx, u_gen, ~renumber_empty_holes=false, e, ty) => {
  let (e, u_gen, _) =
    ana_fix_holes'(ctx, u_gen, ~renumber_empty_holes, e, ty);
  (e, u_gen);
};
let ana_fix_holes_block = (ctx, u_gen, ~renumber_empty_holes=false, block, ty) => {
  let (block, u_gen, _) =
    Lazy.force(
      ana_fix_holes_block',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      block,
      ty,
    );
  (block, u_gen);
};
let ana_fix_holes_opseq = (ctx, u_gen, ~renumber_empty_holes=false, opseq, ty) => {
  let (block, u_gen, _) =
    Lazy.force(
      ana_fix_holes_opseq',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      opseq,
      ty,
    );
  (block, u_gen);
};
let ana_fix_holes_rules =
    (ctx, u_gen, ~renumber_empty_holes=false, rules, pat_ty, clause_ty) => {
  let (block, u_gen, _) =
    Lazy.force(
      ana_fix_holes_rules',
      ctx,
      u_gen,
      ~renumber_empty_holes,
      ~pat_ty,
      rules,
      clause_ty,
    );
  (block, u_gen);
};

let syn_fix_holes_z =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t)
    : (ZExp.t, HTyp.t, MetaVarGen.t) => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, ty, u_gen) = syn_fix_holes(ctx, u_gen, ZExp.erase(ze));
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (ze, ty, u_gen);
};

let syn_fix_holes_zlines =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zlines: ZExp.zblock)
    : (ZExp.zblock, Contexts.t, MetaVarGen.t) => {
  let path = CursorPath_Exp.of_zblock(zlines);
  let (lines, ctx, u_gen, _) =
    Lazy.force(
      syn_fix_holes_lines',
      ctx,
      u_gen,
      ~renumber_empty_holes=false,
      ZExp.erase_zblock(zlines),
    );
  let zlines =
    CursorPath_Exp.follow_block(path, lines)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes_lines did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (zlines, ctx, u_gen);
};

let syn_fix_holes_zrules =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      zrules: ZExp.zrules,
      pat_ty: HTyp.t,
    )
    : (ZExp.zrules, list(HTyp.t), option(HTyp.t), MetaVarGen.t) => {
  let path = CursorPath_Exp.of_zrules(zrules);
  let rules = ZExp.erase_zrules(zrules);
  let (rules, u_gen, rule_types, common_type) =
    syn_fix_holes_rules(ctx, u_gen, rules, pat_ty);
  let zrules =
    CursorPath_Exp.follow_rules(path, rules)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes_rules did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (zrules, rule_types, common_type, u_gen);
};

let ana_fix_holes_z =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, ze: ZExp.t, ty: HTyp.t)
    : (ZExp.t, MetaVarGen.t) => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, u_gen) = ana_fix_holes(ctx, u_gen, ZExp.erase(ze), ty);
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "ana_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (ze, u_gen);
};

/* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
let fix_and_renumber_holes =
    (ctx: Contexts.t, e: UHExp.t): (UHExp.t, HTyp.t, MetaVarGen.t) =>
  syn_fix_holes(ctx, MetaVarGen.init, ~renumber_empty_holes=true, e);

let fix_and_renumber_holes_z =
    (ctx: Contexts.t, ze: ZExp.t): Statics_common.edit_state => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, ty, u_gen) = fix_and_renumber_holes(ctx, ZExp.erase(ze));
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "fix_and_renumber_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath_common.sexp_of_t(path)),
         )
       );
  (ze, ty, u_gen);
};
