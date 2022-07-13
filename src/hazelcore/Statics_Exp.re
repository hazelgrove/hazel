open OptUtil.Syntax;

let tuple_zip =
  Statics_common.tuple_zip(~get_tuple_elements=UHExp.get_tuple_elements);

let recursive_let_id =
    (ctx: Context.t, p: UHPat.t, def: UHExp.t): option(Var.t) =>
  switch (p, def) {
  | (
      OpSeq(_, S(TypeAnn(_, Var(_, NotInVarHole, x), _), E)),
      [ExpLine(OpSeq(_, S(Fun(_), E)))],
    ) =>
    open OptUtil.Syntax;
    let* (ty_p, ctx_p) = Statics_Pat.syn(ctx, p);
    let+ _ = HTyp.matched_arrow(ctx_p, ty_p);
    x;
  | _ => None
  };

let extend_let_def_ctx = (ctx: Context.t, p: UHPat.t, def: UHExp.t): Context.t =>
  {
    open OptUtil.Syntax;
    let* id = recursive_let_id(ctx, p, def);
    let+ (ty_p, _) = Statics_Pat.syn(ctx, p);
    let ty_p = HTyp.rescope(ctx, ty_p);
    Context.add_var(ctx, id, ty_p);
  }
  |> Option.value(~default=ctx);

let get_pattern_type = (ctx, UHExp.Rule(p, _)) =>
  p |> Statics_Pat.syn(ctx) |> Option.map(((ty, _)) => ty);

let joined_pattern_type = (ctx, rules) => {
  let* tys = rules |> List.map(get_pattern_type(ctx)) |> OptUtil.sequence;
  HTyp.join_all(ctx, LUB, tys);
};

let rec syn = (ctx: Context.t, e: UHExp.t): option(HTyp.t) =>
  syn_block(ctx, e)

and syn_block = (ctx: Context.t, block: UHExp.block): option(HTyp.t) => {
  let* (leading, conclusion) = UHExp.Block.split_conclusion(block);
  let* new_ctx = syn_lines(ctx, leading);
  let+ ty = syn_opseq(new_ctx, conclusion);
  Context.reduce_tyvars(new_ctx, ctx, ty);
}

and syn_lines = (ctx: Context.t, lines: list(UHExp.line)): option(Context.t) =>
  lines
  |> List.fold_left(
       (opt_ctx: option(Context.t), line: UHExp.line) => {
         let* ctx = opt_ctx;
         syn_line(ctx, line);
       },
       Some(ctx),
     )

and syn_line = (ctx: Context.t, line: UHExp.line): option(Context.t) =>
  switch (line) {
  | EmptyLine
  | CommentLine(_) => Some(ctx)
  | ExpLine(opseq) =>
    let+ _ = syn_opseq(ctx, opseq);
    ctx;
  | LetLine(p, def) =>
    let def_ctx = extend_let_def_ctx(ctx, p, def);
    let* def_ty = syn(def_ctx, def);
    let ty = HTyp.rescope(ctx, def_ty);
    Statics_Pat.ana(ctx, p, ty);
  | TyAliasLine(tp, ty) =>
    open OptUtil.Syntax;
    let+ (_, k, _) = Elaborator_Typ.syn_elab(ctx, Delta.empty, ty);
    Statics_TPat.ana(ctx, tp, k);
  }

and syn_opseq =
    (ctx: Context.t, OpSeq(skel, seq): UHExp.opseq): option(HTyp.t) =>
  syn_skel(ctx, skel, seq)

and syn_skel =
    (ctx: Context.t, skel: UHExp.skel, seq: UHExp.seq): option(HTyp.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = Seq.nth_operand(n, seq);
    syn_operand(ctx, en);
  | BinOp(InHole(_), op, skel1, skel2) =>
    let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
    let+ _ = syn_skel(ctx, skel_not_in_hole, seq);
    HTyp.hole();
  | BinOp(NotInHole, Minus | Plus | Times | Divide, skel1, skel2) =>
    let+ _ = ana_skel(ctx, skel1, seq, HTyp.int())
    and+ _ = ana_skel(ctx, skel2, seq, HTyp.int());
    HTyp.int();
  | BinOp(NotInHole, FMinus | FPlus | FTimes | FDivide, skel1, skel2) =>
    let+ _ = ana_skel(ctx, skel1, seq, HTyp.float())
    and+ _ = ana_skel(ctx, skel2, seq, HTyp.float());
    HTyp.float();
  | BinOp(NotInHole, And | Or, skel1, skel2) =>
    let+ _ = ana_skel(ctx, skel1, seq, HTyp.bool())
    and+ _ = ana_skel(ctx, skel2, seq, HTyp.bool());
    HTyp.bool();
  | BinOp(NotInHole, LessThan | GreaterThan | Equals, skel1, skel2) =>
    let+ _ = ana_skel(ctx, skel1, seq, HTyp.int())
    and+ _ = ana_skel(ctx, skel2, seq, HTyp.int());
    HTyp.bool();
  | BinOp(NotInHole, FLessThan | FGreaterThan | FEquals, skel1, skel2) =>
    let+ _ = ana_skel(ctx, skel1, seq, HTyp.float())
    and+ _ = ana_skel(ctx, skel2, seq, HTyp.float());
    HTyp.bool();
  | BinOp(NotInHole, Space, skel1, skel2) =>
    let* ty1 = syn_skel(ctx, skel1, seq);
    let* (ty2, ty) = HTyp.matched_arrow(ctx, ty1);
    let+ _ = ana_skel(ctx, skel2, seq, ty2);
    ty;
  | BinOp(NotInHole, Comma, _, _) =>
    skel
    |> UHExp.get_tuple_elements
    |> List.map(skel => syn_skel(ctx, skel, seq))
    |> OptUtil.sequence
    |> Option.map(tys => HTyp.product(tys))
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* ty1 = syn_skel(ctx, skel1, seq);
    let ty = HTyp.list(ty1);
    let+ _ = ana_skel(ctx, skel2, seq, ty);
    ty;
  }

and syn_operand = (ctx: Context.t, operand: UHExp.operand): option(HTyp.t) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some(HTyp.hole())
  | InvalidText(_) => Some(HTyp.hole())
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Fun(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    let+ _ = syn_operand(ctx, operand');
    HTyp.hole();
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Fun(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _) => None
  | Case(InconsistentBranches(rule_types, _), scrut, rules) =>
    let* pat_ty = syn(ctx, scrut);
    /* Make sure the rule synthesizes the type the rule_types says it does */
    let correct_rule_types =
      List.for_all2(
        (rule_ty, rule) => {
          switch (syn_rule(ctx, rule, pat_ty)) {
          | None => false
          | Some(syn_ty) => HTyp.equivalent(ctx, rule_ty, syn_ty)
          }
        },
        rule_types,
        rules,
      );
    correct_rule_types ? Some(HTyp.hole()) : None;
  /* not in hole */
  | Var(NotInHole, NotInVarHole, x) => Context.var_type(ctx, x)
  | Var(NotInHole, InVarHole(_), _) => Some(HTyp.hole())
  | IntLit(NotInHole, _) => Some(HTyp.int())
  | FloatLit(NotInHole, _) => Some(HTyp.float())
  | BoolLit(NotInHole, _) => Some(HTyp.bool())
  | ListNil(NotInHole) => Some(HTyp.list(HTyp.hole()))
  | Fun(NotInHole, p, body) =>
    let* (ty_p, body_ctx) = Statics_Pat.syn(ctx, p);
    let+ ty_body = syn(body_ctx, body);
    HTyp.arrow(ty_p, ty_body);
  | Inj(NotInHole, side, body) =>
    let+ ty = syn(ctx, body);
    switch (side) {
    | L => HTyp.sum(ty, HTyp.hole())
    | R => HTyp.sum(HTyp.hole(), ty)
    };
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    let* clause_ty = syn(ctx, scrut);
    syn_rules(ctx, rules, clause_ty);
  | Parenthesized(body) => syn(ctx, body)
  }

and syn_rules =
    (ctx: Context.t, rules: UHExp.rules, pat_ty: HTyp.t): option(HTyp.t) => {
  let* clause_types =
    List.fold_left(
      (types_opt, r) => {
        let* types = types_opt;
        let+ r_ty = syn_rule(ctx, r, pat_ty);
        [r_ty, ...types];
      },
      Some([]),
      rules,
    );
  HTyp.join_all(ctx, GLB, clause_types);
}

and syn_rule =
    (ctx: Context.t, rule: UHExp.rule, pat_ty: HTyp.t): option(HTyp.t) => {
  let Rule(p, clause) = rule;
  let* ctx = Statics_Pat.ana(ctx, p, pat_ty);
  syn(ctx, clause);
}

and ana = (ctx: Context.t, e: UHExp.t, ty: HTyp.t): option(unit) =>
  ana_block(ctx, e, ty)

and ana_block =
    (ctx: Context.t, block: UHExp.block, ty: HTyp.t): option(unit) => {
  let* (leading, conclusion) = UHExp.Block.split_conclusion(block);
  let* new_ctx = syn_lines(ctx, leading);
  ana_opseq(new_ctx, conclusion, ty);
}

and ana_opseq =
    (ctx: Context.t, OpSeq(skel, seq) as opseq: UHExp.opseq, ty: HTyp.t)
    : option(unit) => {
  let ty_head_normed = HTyp.head_normalize(ctx, ty);
  switch (tuple_zip(skel, ty_head_normed)) {
  | None =>
    switch (
      UHExp.get_err_status_opseq(opseq),
      HTyp.get_prod_elements(ty_head_normed),
    ) {
    | (InHole(TypeInconsistent, _), [_])
    | (InHole(WrongLength, _), _) =>
      let opseq' = UHExp.set_err_status_opseq(NotInHole, opseq);
      let+ _ = syn_opseq(ctx, opseq');
      ();
    | _ => None
    }
  | Some(skel_tys) =>
    let+ _ =
      skel_tys
      |> List.map(((skel, ty)) => ana_skel(ctx, skel, seq, ty))
      |> OptUtil.sequence;
    ();
  };
}

and ana_skel =
    (ctx: Context.t, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
    : option(unit) =>
  switch (skel) {
  | BinOp(_, Comma, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    failwith(__LOC__ ++ ": tuples handled at opseq level")
  | Placeholder(n) =>
    let en = Seq.nth_operand(n, seq);
    ana_operand(ctx, en, ty);
  | BinOp(NotInHole, Cons, skel1, skel2) =>
    let* ty_elt = HTyp.matched_list(ctx, ty);
    let* _ = ana_skel(ctx, skel1, seq, ty_elt);
    ana_skel(ctx, skel2, seq, HTyp.list(ty_elt));
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
    let* ty' = syn_skel(ctx, skel, seq);
    HTyp.consistent(ctx, ty, ty') ? Some() : None;
  }

and ana_operand =
    (ctx: Context.t, operand: UHExp.operand, ty: HTyp.t): option(unit) =>
  switch (operand) {
  /* in hole */
  | EmptyHole(_) => Some()
  | InvalidText(_) => Some()
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Fun(InHole(TypeInconsistent, _), _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    let+ _ = syn_operand(ctx, operand');
    (); /* this is a consequence of subsumption and hole universality */
  | Var(InHole(WrongLength, _), _, _)
  | IntLit(InHole(WrongLength, _), _)
  | FloatLit(InHole(WrongLength, _), _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(InHole(WrongLength, _))
  | Fun(InHole(WrongLength, _), _, _)
  | Inj(InHole(WrongLength, _), _, _)
  | Case(StandardErrStatus(InHole(WrongLength, _)), _, _) =>
    HTyp.head_normalize(ctx, ty) |> HTyp.get_prod_elements |> List.length > 1
      ? Some() : None
  | Case(InconsistentBranches(_, _), _, _) => None
  /* not in hole */
  | ListNil(NotInHole) =>
    let+ _ = HTyp.matched_list(ctx, ty);
    ();
  | Var(NotInHole, _, _)
  | IntLit(NotInHole, _)
  | FloatLit(NotInHole, _)
  | BoolLit(NotInHole, _) =>
    let operand' = UHExp.set_err_status_operand(NotInHole, operand);
    let* ty' = syn_operand(ctx, operand');
    HTyp.consistent(ctx, ty, ty') ? Some() : None;
  | Fun(NotInHole, p, body) =>
    let* (ty_p_given, ty_body) = HTyp.matched_arrow(ctx, ty);
    let* ctx_body = Statics_Pat.ana(ctx, p, ty_p_given);
    ana(ctx_body, body, ty_body);
  | Inj(NotInHole, side, body) =>
    let* (ty1, ty2) = HTyp.matched_sum(ctx, ty);
    ana(ctx, body, InjSide.pick(side, ty1, ty2));
  | Case(StandardErrStatus(NotInHole), scrut, rules) =>
    let* ty1 = syn(ctx, scrut);
    ana_rules(ctx, rules, ty1, ty);
  | Parenthesized(body) => ana(ctx, body, ty)
  }

and ana_rules =
    (ctx: Context.t, rules: UHExp.rules, pat_ty: HTyp.t, clause_ty: HTyp.t)
    : option(unit) =>
  List.fold_left(
    (b, r) => {
      let* _ = b;
      ana_rule(ctx, r, pat_ty, clause_ty);
    },
    Some(),
    rules,
  )

and ana_rule =
    (
      ctx: Context.t,
      Rule(p, clause): UHExp.rule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : option(unit) => {
  let* ctx = Statics_Pat.ana(ctx, p, pat_ty);
  ana(ctx, clause, clause_ty);
};

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let rec syn_nth_type_mode =
        (ctx: Context.t, n: int, OpSeq(skel, seq): UHExp.opseq)
        : option(Statics.type_mode) =>
  syn_nth_type_mode'(ctx, n, skel, seq)

and syn_nth_type_mode' =
    (ctx: Context.t, n: int, skel: UHExp.skel, seq: UHExp.seq)
    : option(Statics.type_mode) => {
  let ana_go = (skel, ty) => ana_nth_type_mode'(ctx, n, skel, seq, ty);
  let rec go = (skel: UHExp.skel) =>
    switch (skel) {
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics.Syn);
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
          let* (ty2, _) = HTyp.matched_arrow(ctx, ty1);
          ana_go(skel2, ty2);
        }
      }
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      let* ty1 = syn_skel(ctx, skel1, seq);
      n <= Skel.rightmost_tm_index(skel1)
        ? go(skel1) : ana_go(skel2, HTyp.list(ty1));
    | BinOp(
        NotInHole,
        Plus | Minus | Times | Divide | LessThan | GreaterThan,
        skel1,
        skel2,
      ) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, HTyp.int()) : ana_go(skel2, HTyp.int())
    | BinOp(
        NotInHole,
        FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan,
        skel1,
        skel2,
      ) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, HTyp.float()) : ana_go(skel2, HTyp.float())
    | BinOp(NotInHole, And | Or, skel1, skel2) =>
      n <= Skel.rightmost_tm_index(skel1)
        ? ana_go(skel1, HTyp.bool()) : ana_go(skel2, HTyp.bool())
    | BinOp(NotInHole, Equals, skel1, skel2) =>
      if (n <= Skel.rightmost_tm_index(skel1)) {
        go(skel1);
      } else {
        let* ty1 = syn_skel(ctx, skel1, seq);
        ana_go(skel2, ty1);
      }
    | BinOp(NotInHole, FEquals, skel1, skel2) =>
      if (n <= Skel.rightmost_tm_index(skel1)) {
        go(skel1);
      } else {
        let* ty1 = syn_skel(ctx, skel1, seq);
        ana_go(skel2, ty1);
      }
    };
  go(skel);
}

/**
 * Get type mode of nth operand of an opseq in analytic position
 */
and ana_nth_type_mode =
    (
      ctx: Context.t,
      n: int,
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      ty: HTyp.t,
    )
    : option(Statics.type_mode) =>
  // handle n-tuples
  switch (tuple_zip(skel, HTyp.head_normalize(ctx, ty))) {
  | None =>
    syn_nth_type_mode(ctx, n, UHExp.set_err_status_opseq(NotInHole, opseq))
  | Some(skel_tys) =>
    let (nskel, nty) =
      skel_tys
      |> List.find(((skel, _)) =>
           Skel.leftmost_tm_index(skel) <= n
           && n <= Skel.rightmost_tm_index(skel)
         );
    ana_nth_type_mode'(ctx, n, nskel, seq, nty);
  }

and ana_nth_type_mode' =
    (ctx: Context.t, n: int, skel: UHExp.skel, seq: UHExp.seq, ty: HTyp.t)
    : option(Statics.type_mode) => {
  let syn_go = skel => syn_nth_type_mode'(ctx, n, skel, seq);
  let rec go = (skel: UHExp.skel, ty: HTyp.t) =>
    switch (skel) {
    | BinOp(_, Comma, _, _)
    | BinOp(InHole(WrongLength, _), _, _, _) =>
      failwith(__LOC__ ++ ": expected tuples to be handled at opseq level")
    | Placeholder(n') =>
      assert(n == n');
      Some(Statics.Ana(ty));
    | BinOp(InHole(TypeInconsistent, _), op, skel1, skel2) =>
      let skel_not_in_hole = Skel.BinOp(NotInHole, op, skel1, skel2);
      syn_go(skel_not_in_hole);
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ctx, ty)) {
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

/* If renumber_empty_holes is true, then the metavars in empty holes will be assigned
 * new values in the same namespace as non-empty holes. Non-empty holes are renumbered
 * regardless.
 */
let rec syn_fix_holes =
        (
          ctx: Context.t,
          id_gen: IDGen.t,
          ~renumber_empty_holes=false,
          e: UHExp.t,
        )
        : (UHExp.t, HTyp.t, IDGen.t) =>
  syn_fix_holes_block(ctx, id_gen, ~renumber_empty_holes, e)

and syn_fix_holes_block =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      block: UHExp.block,
    )
    : (UHExp.block, HTyp.t, IDGen.t) =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None =>
    let (leading, _ctx, id_gen) =
      syn_fix_holes_lines(ctx, id_gen, ~renumber_empty_holes, block);
    let (conclusion, id_gen) = id_gen |> UHExp.new_EmptyHole;
    (
      leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)],
      HTyp.hole(),
      id_gen,
    );
  | Some((leading, conclusion)) =>
    let (leading, ctx_suffix, id_gen) =
      syn_fix_holes_lines(ctx, id_gen, ~renumber_empty_holes, leading);
    let (conclusion, ty, id_gen) =
      syn_fix_holes_opseq(
        ctx_suffix,
        id_gen,
        ~renumber_empty_holes,
        conclusion,
      );
    let ty = Context.reduce_tyvars(ctx_suffix, ctx, ty);
    (leading @ [UHExp.ExpLine(conclusion)], ty, id_gen);
  }

and syn_fix_holes_lines =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      lines: list(UHExp.line),
    )
    : (list(UHExp.line), Context.t, IDGen.t) => {
  let (rev_fixed_lines, ctx, id_gen) =
    lines
    |> List.fold_left(
         (
           (fixed_lines, ctx, id_gen): (
             list(UHExp.line),
             Context.t,
             IDGen.t,
           ),
           line: UHExp.line,
         ) => {
           let (fixed_line, ctx, id_gen) =
             syn_fix_holes_line(ctx, id_gen, ~renumber_empty_holes, line);
           ([fixed_line, ...fixed_lines], ctx, id_gen);
         },
         ([], ctx, id_gen),
       );
  (rev_fixed_lines |> List.rev, ctx, id_gen);
}

and syn_fix_holes_line =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      line: UHExp.line,
    )
    : (UHExp.line, Context.t, IDGen.t) =>
  switch (line) {
  | ExpLine(e) =>
    let (e, _, id_gen) =
      syn_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, e);
    (ExpLine(e), ctx, id_gen);
  | EmptyLine
  | CommentLine(_) => (line, ctx, id_gen)
  | TyAliasLine(p, ty) =>
    let (ty, _, id_gen) = Statics_UHTyp.syn_fix_holes(ctx, id_gen, ty);
    switch (Elaborator_Typ.syn_elab(ctx, Delta.empty, ty)) {
    | Some((_, kind, _)) =>
      let (ctx, p, id_gen) = Statics_TPat.fix_holes(ctx, p, kind, id_gen);
      (TyAliasLine(p, ty), ctx, id_gen);
    | None =>
      let (ctx, p, id_gen) = Statics_TPat.fix_holes(ctx, p, Hole, id_gen);
      (TyAliasLine(p, ty), ctx, id_gen);
    };
  | LetLine(p, def) =>
    let (p, ty_p, _, id_gen) =
      Statics_Pat.syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p);
    let def_ctx = extend_let_def_ctx(ctx, p, def);
    let (def, id_gen) =
      ana_fix_holes(def_ctx, id_gen, ~renumber_empty_holes, def, ty_p);
    let body_ctx = extend_let_body_ctx(ctx, p, def);
    (LetLine(p, def), body_ctx, id_gen);
  }

and syn_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq): UHExp.opseq,
    )
    : (UHExp.opseq, HTyp.t, IDGen.t) => {
  let (skel, seq, ty, id_gen) =
    syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq);
  (OpSeq(skel, seq), ty, id_gen);
}

and syn_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHExp.skel,
      seq: UHExp.seq,
    )
    : (UHExp.skel, UHExp.seq, HTyp.t, IDGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, ty, id_gen) =
      syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, en);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, ty, id_gen);
  | BinOp(_, (Minus | Plus | Times | Divide) as op, skel1, skel2) =>
    let (skel1, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.int(),
      );
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.int(),
      );
    (BinOp(NotInHole, op, skel1, skel2), seq, HTyp.int(), id_gen);
  | BinOp(_, (FMinus | FPlus | FTimes | FDivide) as op, skel1, skel2) =>
    let (skel1, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.float(),
      );
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.float(),
      );
    (BinOp(NotInHole, op, skel1, skel2), seq, HTyp.float(), id_gen);
  | BinOp(_, (And | Or) as op, skel1, skel2) =>
    let (skel1, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.bool(),
      );
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.bool(),
      );
    (BinOp(NotInHole, op, skel1, skel2), seq, HTyp.bool(), id_gen);
  | BinOp(_, (LessThan | GreaterThan | Equals) as op, skel1, skel2) =>
    let (skel1, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.int(),
      );
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.int(),
      );
    (BinOp(NotInHole, op, skel1, skel2), seq, HTyp.bool(), id_gen);
  | BinOp(_, (FLessThan | FGreaterThan | FEquals) as op, skel1, skel2) =>
    let (skel1, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel1,
        seq,
        HTyp.float(),
      );
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        skel2,
        seq,
        HTyp.float(),
      );
    (BinOp(NotInHole, op, skel1, skel2), seq, HTyp.bool(), id_gen);
  | BinOp(_, Space, skel1, skel2) =>
    let (skel1, seq, ty1, id_gen) =
      syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
    switch (HTyp.matched_arrow(ctx, ty1)) {
    | Some((ty2, ty)) =>
      let (skel2, seq, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty2,
        );
      (BinOp(NotInHole, Space, skel1, skel2), seq, ty, id_gen);
    | None =>
      let (skel2, seq, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          HTyp.hole(),
        );
      let (OpSeq(skel1, seq), id_gen) =
        UHExp.mk_inconsistent_opseq(id_gen, OpSeq(skel1, seq));
      (BinOp(NotInHole, Space, skel1, skel2), seq, HTyp.hole(), id_gen);
    };
  | BinOp(_, Comma, _, _) =>
    let ((id_gen, seq), pairs) =
      skel
      |> UHExp.get_tuple_elements
      |> ListUtil.map_with_accumulator(
           ((id_gen, seq), skel) => {
             let (skel, seq, ty, id_gen) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ((id_gen, seq), (skel, ty));
           },
           (id_gen, seq),
         );
    let (skels, tys) = List.split(pairs);
    (UHExp.mk_tuple(skels), seq, HTyp.product(tys), id_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    let (skel1, seq, ty_elt, id_gen) =
      syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
    let ty = HTyp.list(ty_elt);
    let (skel2, seq, id_gen) =
      ana_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel2, seq, ty);
    let skel = Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
    (skel, seq, ty, id_gen);
  }

and syn_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.operand,
    )
    : (UHExp.operand, HTyp.t, IDGen.t) => {
  let e_nih = UHExp.set_err_status_operand(NotInHole, e);
  switch (e) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), HTyp.hole(), id_gen);
    } else {
      (e, HTyp.hole(), id_gen);
    }
  | InvalidText(_) => (e, HTyp.hole(), id_gen)
  | Var(_, var_err_status, x) =>
    switch (Context.var_type(ctx, x)) {
    | Some(ty) => (UHExp.Var(NotInHole, NotInVarHole, x), ty, id_gen)
    | None =>
      switch (var_err_status) {
      | InVarHole(_, _) => (e_nih, HTyp.hole(), id_gen)
      | NotInVarHole =>
        let (u, id_gen) = IDGen.next_hole(id_gen);
        let reason: VarErrStatus.HoleReason.t =
          switch (ExpandingKeyword.mk(x)) {
          | Some(t) => Keyword(t)
          | None => Free
          };
        (Var(NotInHole, InVarHole(reason, u), x), HTyp.hole(), id_gen);
      }
    }
  | IntLit(_, _) => (e_nih, HTyp.int(), id_gen)
  | FloatLit(_, _) => (e_nih, HTyp.float(), id_gen)
  | BoolLit(_, _) => (e_nih, HTyp.bool(), id_gen)
  | ListNil(_) => (e_nih, HTyp.list(HTyp.hole()), id_gen)
  | Parenthesized(body) =>
    let (block, ty, id_gen) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, body);
    (Parenthesized(block), ty, id_gen);
  | Fun(_, p, body) =>
    let (p, ty_p, ctx_body, id_gen) =
      Statics_Pat.syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, p);
    let (body, ty_body, id_gen) =
      syn_fix_holes(ctx_body, id_gen, ~renumber_empty_holes, body);
    (Fun(NotInHole, p, body), HTyp.arrow(ty_p, ty_body), id_gen);
  | Inj(_, side, body) =>
    let (body, ty1, id_gen) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, body);
    let ty =
      switch (side) {
      | L => HTyp.sum(ty1, HTyp.hole())
      | R => HTyp.sum(HTyp.hole(), ty1)
      };
    (Inj(NotInHole, side, body), ty, id_gen);
  | Case(_, scrut, rules) =>
    let (scrut, ty1, id_gen) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, scrut);
    let (rules, id_gen, rule_types, common_type) =
      syn_fix_holes_rules(ctx, id_gen, ~renumber_empty_holes, rules, ty1);
    switch (common_type) {
    | None =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        Case(InconsistentBranches(rule_types, u), scrut, rules),
        HTyp.hole(),
        id_gen,
      );
    | Some(common_type) => (
        Case(StandardErrStatus(NotInHole), scrut, rules),
        common_type,
        id_gen,
      )
    };
  };
}

and syn_fix_holes_rules =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      rules: UHExp.rules,
      pat_ty: HTyp.t,
    )
    : (UHExp.rules, IDGen.t, list(HTyp.t), option(HTyp.t)) => {
  let (rev_fixed_rules, id_gen, rule_types) =
    List.fold_left(
      ((rules, id_gen, rule_types), r) => {
        let (r, id_gen, r_ty) =
          syn_fix_holes_rule(ctx, id_gen, ~renumber_empty_holes, r, pat_ty);
        ([r, ...rules], id_gen, [r_ty, ...rule_types]);
      },
      ([], id_gen, []),
      rules,
    );
  let common_type = HTyp.join_all(ctx, GLB, rule_types);
  (List.rev(rev_fixed_rules), id_gen, List.rev(rule_types), common_type);
}

and syn_fix_holes_rule =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      rule: UHExp.rule,
      pat_ty: HTyp.t,
    )
    : (UHExp.rule, IDGen.t, HTyp.t) => {
  let Rule(p, clause) = rule;
  let (p, ctx, id_gen) =
    Statics_Pat.ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p, pat_ty);
  let (clause, clause_ty, id_gen) =
    syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, clause);
  (Rule(p, clause), id_gen, clause_ty);
}

and ana_fix_holes_rules =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      rules: UHExp.rules,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : (UHExp.rules, IDGen.t) => {
  let (rev_fixed_rules, id_gen) =
    List.fold_left(
      ((rules, id_gen), r) => {
        let (r, id_gen) =
          ana_fix_holes_rule(
            ctx,
            id_gen,
            ~renumber_empty_holes,
            r,
            pat_ty,
            clause_ty,
          );
        ([r, ...rules], id_gen);
      },
      ([], id_gen),
      rules,
    );
  (List.rev(rev_fixed_rules), id_gen);
}

and ana_fix_holes_rule =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      Rule(p, clause): UHExp.rule,
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : (UHExp.rule, IDGen.t) => {
  let (p, ctx, id_gen) =
    Statics_Pat.ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, p, pat_ty);
  let (clause, id_gen) =
    ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, clause, clause_ty);
  (Rule(p, clause), id_gen);
}

and ana_fix_holes =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.t,
      ty: HTyp.t,
    )
    : (UHExp.t, IDGen.t) =>
  ana_fix_holes_block(ctx, id_gen, ~renumber_empty_holes, e, ty)

and ana_fix_holes_block =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      block: UHExp.block,
      ty: HTyp.t,
    )
    : (UHExp.block, IDGen.t) =>
  switch (block |> UHExp.Block.split_conclusion) {
  | None =>
    let (leading, _ctx, id_gen) =
      syn_fix_holes_lines(ctx, id_gen, ~renumber_empty_holes, block);
    let (conclusion, id_gen) = id_gen |> UHExp.new_EmptyHole;
    (leading @ [UHExp.ExpLine(conclusion |> OpSeq.wrap)], id_gen);
  | Some((leading, conclusion)) =>
    let (leading, ctx, id_gen) =
      syn_fix_holes_lines(ctx, id_gen, ~renumber_empty_holes, leading);
    let (conclusion, id_gen) =
      ana_fix_holes_opseq(ctx, id_gen, ~renumber_empty_holes, conclusion, ty);
    (leading @ [UHExp.ExpLine(conclusion)], id_gen);
  }

and ana_fix_holes_opseq =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      ty: HTyp.t,
    )
    : (UHExp.opseq, IDGen.t) => {
  let ty_head_normed = HTyp.head_normalize(ctx, ty);
  // handle n-tuples
  switch (tuple_zip(skel, ty_head_normed)) {
  | Some(skel_tys) =>
    skel_tys
    |> List.fold_left(
         (
           (rev_skels: list(UHExp.skel), seq: UHExp.seq, id_gen: IDGen.t),
           (skel: UHExp.skel, ty: HTyp.t),
         ) => {
           let (skel, seq, id_gen) =
             ana_fix_holes_skel(
               ctx,
               id_gen,
               ~renumber_empty_holes,
               skel,
               seq,
               ty,
             );
           ([skel, ...rev_skels], seq, id_gen);
         },
         ([], seq, id_gen),
       )
    |> (
      fun
      | (rev_skels, seq, id_gen) => {
          let skel = rev_skels |> List.rev |> UHExp.mk_tuple;
          (OpSeq.OpSeq(skel, seq), id_gen);
        }
    )
  | None =>
    if (List.length(HTyp.get_prod_elements(ty_head_normed)) == 1) {
      skel
      |> UHExp.get_tuple_elements
      |> List.fold_left(
           (
             (rev_skels: list(UHExp.skel), seq: UHExp.seq, id_gen: IDGen.t),
             skel: UHExp.skel,
           ) => {
             let (skel, seq, _, id_gen) =
               syn_fix_holes_skel(
                 ctx,
                 id_gen,
                 ~renumber_empty_holes,
                 skel,
                 seq,
               );
             ([skel, ...rev_skels], seq, id_gen);
           },
           ([], seq, id_gen),
         )
      |> (
        fun
        | (rev_skels, seq, id_gen) => {
            let (u, id_gen) = IDGen.next_hole(id_gen);
            let skel = UHExp.mk_tuple(List.rev(rev_skels));
            let opseq =
              UHExp.set_err_status_opseq(
                InHole(TypeInconsistent, u),
                OpSeq.OpSeq(skel, seq),
              );
            (opseq, id_gen);
          }
      );
    } else {
      let (u, id_gen) = id_gen |> IDGen.next_hole;
      let (opseq, _, id_gen) =
        syn_fix_holes_opseq(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          opseq |> UHExp.set_err_status_opseq(NotInHole),
        );
      (opseq |> UHExp.set_err_status_opseq(InHole(WrongLength, u)), id_gen);
    }
  };
}

and ana_fix_holes_skel =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      skel: UHExp.skel,
      seq: UHExp.seq,
      ty: HTyp.t,
    )
    : (UHExp.skel, UHExp.seq, IDGen.t) =>
  switch (skel) {
  | BinOp(_, Comma, _, _) =>
    failwith("Exp.ana_fix_holes_skel: tuples handled at opseq level")
  | Placeholder(n) =>
    let en = seq |> Seq.nth_operand(n);
    let (en, id_gen) =
      ana_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, en, ty);
    let seq = seq |> Seq.update_nth_operand(n, en);
    (skel, seq, id_gen);
  | BinOp(_, Cons, skel1, skel2) =>
    switch (HTyp.matched_list(ctx, ty)) {
    | Some(ty_elt) =>
      let (skel1, seq, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel1,
          seq,
          ty_elt,
        );
      let ty_list = HTyp.list(ty_elt);
      let (skel2, seq, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let skel = Skel.BinOp(NotInHole, Operators_Exp.Cons, skel1, skel2);
      (skel, seq, id_gen);
    | None =>
      let (skel1, seq, ty_elt, id_gen) =
        syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel1, seq);
      let ty_list = HTyp.list(ty_elt);
      let (skel2, seq, id_gen) =
        ana_fix_holes_skel(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          skel2,
          seq,
          ty_list,
        );
      let (u, id_gen) = IDGen.next_hole(id_gen);
      let skel =
        Skel.BinOp(
          InHole(TypeInconsistent, u),
          Operators_Exp.Cons,
          skel1,
          skel2,
        );
      (skel, seq, id_gen);
    }
  | BinOp(
      _,
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
    let (skel, seq, ty', id_gen) =
      syn_fix_holes_skel(ctx, id_gen, ~renumber_empty_holes, skel, seq);
    if (HTyp.consistent(ctx, ty, ty')) {
      (skel, seq, id_gen);
    } else {
      let (OpSeq(skel, seq), id_gen) =
        UHExp.mk_inconsistent_opseq(id_gen, OpSeq(skel, seq));
      (skel, seq, id_gen);
    };
  }

and ana_fix_holes_operand =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      ~renumber_empty_holes=false,
      e: UHExp.operand,
      ty: HTyp.t,
    )
    : (UHExp.operand, IDGen.t) =>
  switch (e) {
  | EmptyHole(_) =>
    if (renumber_empty_holes) {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (EmptyHole(u), id_gen);
    } else {
      (e, id_gen);
    }
  | InvalidText(_) => (e, id_gen)
  | Var(_, _, _)
  | IntLit(_, _)
  | FloatLit(_, _)
  | BoolLit(_, _) =>
    let (e, ty', id_gen) =
      syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, e);
    if (HTyp.consistent(ctx, ty, ty')) {
      (UHExp.set_err_status_operand(NotInHole, e), id_gen);
    } else {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e),
        id_gen,
      );
    };
  | ListNil(_) =>
    switch (HTyp.matched_list(ctx, ty)) {
    | Some(_) => (UHExp.set_err_status_operand(NotInHole, e), id_gen)
    | None =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (ListNil(InHole(TypeInconsistent, u)), id_gen);
    }
  | Parenthesized(body) =>
    let (body, id_gen) =
      ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, body, ty);
    (Parenthesized(body), id_gen);
  | Fun(_, p, def) =>
    switch (HTyp.matched_arrow(ctx, ty)) {
    | Some((ty1_given, ty2)) =>
      let (p, ctx, id_gen) =
        Statics_Pat.ana_fix_holes(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          p,
          ty1_given,
        );
      let (def, id_gen) =
        ana_fix_holes(ctx, id_gen, ~renumber_empty_holes, def, ty2);
      (UHExp.Fun(NotInHole, p, def), id_gen);
    | None =>
      let (e', _, id_gen) =
        syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, e);
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (
        UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
        id_gen,
      );
    }
  | Inj(_, side, body) =>
    switch (HTyp.matched_sum(ctx, ty)) {
    | Some((ty1, ty2)) =>
      let (e1, id_gen) =
        ana_fix_holes(
          ctx,
          id_gen,
          ~renumber_empty_holes,
          body,
          InjSide.pick(side, ty1, ty2),
        );
      (Inj(NotInHole, side, e1), id_gen);
    | None =>
      let (e', ty', id_gen) =
        syn_fix_holes_operand(ctx, id_gen, ~renumber_empty_holes, e);
      if (HTyp.consistent(ctx, ty, ty')) {
        (UHExp.set_err_status_operand(NotInHole, e'), id_gen);
      } else {
        let (u, id_gen) = IDGen.next_hole(id_gen);
        (
          UHExp.set_err_status_operand(InHole(TypeInconsistent, u), e'),
          id_gen,
        );
      };
    }
  | Case(_, scrut, rules) =>
    let (scrut, scrut_ty, id_gen) =
      syn_fix_holes(ctx, id_gen, ~renumber_empty_holes, scrut);
    let (rules, id_gen) =
      ana_fix_holes_rules(
        ctx,
        id_gen,
        ~renumber_empty_holes,
        rules,
        scrut_ty,
        ty,
      );
    (Case(StandardErrStatus(NotInHole), scrut, rules), id_gen);
  }

and extend_let_body_ctx =
    (ctx: Context.t, p: UHPat.t, def: UHExp.t): Context.t => {
  /* precondition: (p)attern and (def)inition have consistent types */

  let def_ctx = extend_let_def_ctx(ctx, p, def);
  def
  |> syn(def_ctx)
  |> OptUtil.get(_ => failwith("extend_let_body_ctx: impossible syn"))
  |> (ty_p => Statics_Pat.ana(ctx, p, ty_p))
  |> OptUtil.get(_ => failwith("extend_let_body_ctx: impossible ana"));
};

let syn_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, ze: ZExp.t): (ZExp.t, HTyp.t, IDGen.t) => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, ty, id_gen) = syn_fix_holes(ctx, id_gen, ZExp.erase(ze));
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ze, ty, id_gen);
};

let syn_fix_holes_zlines =
    (ctx: Context.t, id_gen: IDGen.t, zlines: ZExp.zblock)
    : (ZExp.zblock, Context.t, IDGen.t) => {
  let path = CursorPath_Exp.of_zblock(zlines);
  let (lines, ctx, id_gen) =
    syn_fix_holes_lines(ctx, id_gen, ZExp.erase_zblock(zlines));
  let zlines =
    CursorPath_Exp.follow_block(path, lines)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes_lines did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zlines, ctx, id_gen);
};

let syn_fix_holes_zrules =
    (ctx: Context.t, id_gen: IDGen.t, zrules: ZExp.zrules, pat_ty: HTyp.t)
    : (ZExp.zrules, list(HTyp.t), option(HTyp.t), IDGen.t) => {
  let path = CursorPath_Exp.of_zrules(zrules);
  let rules = ZExp.erase_zrules(zrules);
  let (rules, id_gen, rule_types, common_type) =
    syn_fix_holes_rules(ctx, id_gen, rules, pat_ty);
  let zrules =
    CursorPath_Exp.follow_rules(path, rules)
    |> OptUtil.get(() =>
         failwith(
           "syn_fix_holes_rules did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zrules, rule_types, common_type, id_gen);
};

let ana_fix_holes_z =
    (ctx: Context.t, id_gen: IDGen.t, ze: ZExp.t, ty: HTyp.t)
    : (ZExp.t, IDGen.t) => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, id_gen) = ana_fix_holes(ctx, id_gen, ZExp.erase(ze), ty);
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "ana_fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ze, id_gen);
};

/* Only to be used on top-level expressions, as it starts hole renumbering at 0 */
let fix_and_renumber_holes =
    (ctx: Context.t, e: UHExp.t): (UHExp.t, HTyp.t, IDGen.t) =>
  syn_fix_holes(ctx, IDGen.init, ~renumber_empty_holes=true, e);

let fix_and_renumber_holes_z =
    (ctx: Context.t, ze: ZExp.t): Statics.edit_state => {
  let path = CursorPath_Exp.of_z(ze);
  let (e, ty, id_gen) = fix_and_renumber_holes(ctx, ZExp.erase(ze));
  let ze =
    CursorPath_Exp.follow(path, e)
    |> OptUtil.get(() =>
         failwith(
           "fix_and_renumber_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (ze, ty, id_gen);
};
