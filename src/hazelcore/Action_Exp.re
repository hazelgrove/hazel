let operator_of_shape = (os: Action.operator_shape): option(UHExp.operator) =>
  switch (os) {
  | SPlus => Some(Plus)
  | SMinus => Some(Minus)
  | STimes => Some(Times)
  | SDivide => Some(Divide)
  | SLessThan => Some(LessThan)
  | SGreaterThan => Some(GreaterThan)
  | SEquals => Some(Equals)
  | SSpace => Some(Space)
  | SComma => Some(Comma)
  | SCons => Some(Cons)
  | SAnd => Some(And)
  | SOr => Some(Or)
  | SArrow
  | SVBar => None
  };

let shape_of_operator = (op: UHExp.operator): option(Action.operator_shape) =>
  switch (op) {
  | Minus => Some(SMinus)
  | Plus => Some(SPlus)
  | Times => Some(STimes)
  | Divide => Some(SDivide)
  | LessThan => Some(SLessThan)
  | GreaterThan => Some(SGreaterThan)
  | Equals => Some(SEquals)
  | Space => Some(SSpace)
  | Comma => Some(SComma)
  | Cons => Some(SCons)
  | And => Some(SAnd)
  | Or => Some(SOr)
  | FPlus
  | FMinus
  | FTimes
  | FDivide
  | FLessThan
  | FGreaterThan
  | FEquals => None
  };

let has_Comma = (ZOpSeq(_, zseq): ZExp.zopseq) =>
  zseq
  |> ZExp.erase_zseq
  |> Seq.operators
  |> List.exists(op => op == Operators_Exp.Comma);

let mk_and_syn_fix_OpSeq =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, seq: UHExp.seq)
    : (UHExp.opseq, HTyp.t, MetaVarGen.t) => {
  let opseq = UHExp.mk_OpSeq(seq);
  Statics_Exp.syn_fix_holes_opseq(ctx, u_gen, opseq);
};
let mk_and_ana_fix_OpSeq =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, seq: UHExp.seq, ty: HTyp.t)
    : (UHExp.opseq, MetaVarGen.t) => {
  let opseq = UHExp.mk_OpSeq(seq);
  Statics_Exp.ana_fix_holes_opseq(ctx, u_gen, opseq, ty);
};
let mk_and_syn_fix_ZOpSeq =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZExp.zseq)
    : (ZExp.t, HTyp.t, MetaVarGen.t) => {
  let zopseq = ZExp.mk_ZOpSeq(zseq);
  Statics_Exp.syn_fix_holes_z(ctx, u_gen, ([], ExpLineZ(zopseq), []));
};
let mk_and_ana_fix_ZOpSeq =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, zseq: ZExp.zseq, ty: HTyp.t)
    : (ZExp.t, MetaVarGen.t) => {
  let zopseq = ZExp.mk_ZOpSeq(zseq);
  Statics_Exp.ana_fix_holes_z(ctx, u_gen, ([], ExpLineZ(zopseq), []), ty);
};

let keyword_action = (kw: ExpandingKeyword.t): Action.t =>
  switch (kw) {
  | Let => Construct(SLet)
  | Case => Construct(SCase)
  };

//TBD
let delete_operator =
  Action_common.delete_operator_(
    ~space=Operators_Exp.Space,
    ~is_EmptyHole=UHExp.is_EmptyHole,
    ~place_before_operand=ZExp.place_before_operand,
    ~place_after_operand=ZExp.place_after_operand,
    ~place_after_operator=ZExp.place_after_operator,
  );

//TBD
let construct_operator_before_zoperand =
  Action_common.construct_operator_before_zoperand_(
    ~is_Space=Operators_Exp.is_Space,
    ~new_EmptyHole=UHExp.new_EmptyHole,
    ~erase_zoperand=ZExp.erase_zoperand,
    ~place_before_operand=ZExp.place_before_operand,
    ~place_after_operator=ZExp.place_after_operator,
  );

//TBD
let construct_operator_after_zoperand =
  Action_common.construct_operator_after_zoperand_(
    ~is_Space=Operators_Exp.is_Space,
    ~new_EmptyHole=UHExp.new_EmptyHole,
    ~erase_zoperand=ZExp.erase_zoperand,
    ~place_before_operand=ZExp.place_before_operand,
    ~place_after_operator=ZExp.place_after_operator,
  );

//TBD
let complete_tuple =
  Action_common.complete_tuple_(
    ~mk_ZOpSeq=ZExp.mk_ZOpSeq,
    ~comma=Operators_Exp.Comma,
    ~zcomma=(OnOp(After), Operators_Exp.Comma),
    ~new_EmptyHole=UHExp.new_EmptyHole,
  );

/**
 * Convert an opseq prefix into standalone
 * line items (0 if prefix is empty, 1 otherwise)
 */
let lines_of_prefix =
    (u_gen: MetaVarGen.t, prefix: UHExp.affix)
    : (list(UHExp.line), MetaVarGen.t) =>
  switch (prefix) {
  | E => ([], u_gen)
  | A(_) =>
    let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
    let opseq = UHExp.mk_OpSeq(Seq.prefix_seq(prefix, S(hole, E)));
    ([UHExp.ExpLine(opseq)], u_gen);
  };

/**
 * Convert an opseq suffix into standalone
 * line items (0 if suffix is empty, 1 otherwise)
 */
let lines_of_suffix =
    (u_gen: MetaVarGen.t, suffix: UHExp.affix)
    : (list(UHExp.line), MetaVarGen.t) =>
  switch (suffix) {
  | E => ([], u_gen)
  | A(_) =>
    let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
    let opseq = UHExp.mk_OpSeq(Seq.seq_suffix(S(hole, E), suffix));
    ([UHExp.ExpLine(opseq)], u_gen);
  };

let resurround =
    (
      u_gen: MetaVarGen.t,
      e: UHExp.t,
      (prefix, suffix): ZExp.operand_surround,
    )
    : (UHExp.t, MetaVarGen.t) =>
  switch (e) {
  | [ExpLine(OpSeq(_, seq))] =>
    let new_seq = Seq.prefix_seq(prefix, Seq.seq_suffix(seq, suffix));
    (UHExp.Block.wrap'(UHExp.mk_OpSeq(new_seq)), u_gen);
  | block =>
    let (prefix_lines, u_gen) = lines_of_prefix(u_gen, prefix);
    let (suffix_lines, u_gen) = lines_of_suffix(u_gen, suffix);
    let new_block = List.concat([prefix_lines, block, suffix_lines]);
    (new_block, u_gen);
  };
let resurround_z =
    (
      u_gen: MetaVarGen.t,
      ze: ZExp.t,
      (prefix, suffix): ZExp.operand_surround,
    )
    : (ZExp.t, MetaVarGen.t) =>
  switch (ze) {
  | (
      [],
      ExpLineZ(
        ZOpSeq(_, ZOperand(zoperand, (inner_prefix, inner_suffix))),
      ),
      [],
    ) =>
    let new_prefix = Seq.affix_affix(inner_prefix, prefix);
    let new_suffix = Seq.affix_affix(inner_suffix, suffix);
    (
      ZExp.ZBlock.wrap'(
        ZExp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      ),
      u_gen,
    );
  | (
      [],
      ExpLineZ(
        ZOpSeq(_, ZOperator(zoperator, (inner_prefix, inner_suffix))),
      ),
      [],
    ) =>
    let new_prefix = Seq.seq_affix(inner_prefix, prefix);
    let new_suffix = Seq.seq_affix(inner_suffix, suffix);
    (
      ZExp.ZBlock.wrap'(
        ZExp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
      ),
      u_gen,
    );
  | (prefix_lines, zline, suffix_lines) =>
    let (new_prefix_lines, u_gen) = {
      let (surround_prefix_lines, u_gen) = lines_of_prefix(u_gen, prefix);
      (surround_prefix_lines @ prefix_lines, u_gen);
    };
    let (new_suffix_lines, u_gen) = {
      let (surround_suffix_lines, u_gen) = lines_of_suffix(u_gen, suffix);
      (suffix_lines @ surround_suffix_lines, u_gen);
    };
    ((new_prefix_lines, zline, new_suffix_lines), u_gen);
  };

type expanding_result = {
  kw: ExpandingKeyword.t,
  u_gen: MetaVarGen.t,
  prefix: list(UHExp.line),
  subject: UHExp.t,
  suffix: list(UHExp.line),
};

type line_success =
  | LineExpands(expanding_result)
  | LineDone((ZExp.zblock, Contexts.t, MetaVarGen.t));

type syn_done = (ZExp.t, HTyp.t, MetaVarGen.t);
type syn_success =
  | SynExpands(expanding_result)
  | SynDone(syn_done);
let mk_SynExpandsToCase = (~u_gen, ~prefix=[], ~suffix=[], ~scrut, ()) =>
  SynExpands({kw: Case, u_gen, prefix, suffix, subject: scrut});
let mk_SynExpandsToLet = (~u_gen, ~prefix=[], ~suffix=[], ~def, ()) =>
  SynExpands({kw: Let, u_gen, prefix, suffix, subject: def});
let wrap_in_SynDone:
  ActionOutcome.t(syn_done) => ActionOutcome.t(syn_success) =
  fun
  | (Failed | CursorEscaped(_)) as err => err
  | Succeeded(syn_done) => Succeeded(SynDone(syn_done));

type ana_done = (ZExp.t, MetaVarGen.t);
type ana_success =
  | AnaExpands(expanding_result)
  | AnaDone(ana_done);
let mk_AnaExpandsToCase = (~u_gen, ~prefix=[], ~suffix=[], ~scrut, ()) =>
  AnaExpands({kw: Case, u_gen, prefix, suffix, subject: scrut});
let mk_AnaExpandsToLet = (~u_gen, ~prefix=[], ~suffix=[], ~def, ()) =>
  AnaExpands({kw: Let, u_gen, prefix, suffix, subject: def});
let wrap_in_AnaDone:
  ActionOutcome.t(ana_done) => ActionOutcome.t(ana_success) =
  fun
  | (Failed | CursorEscaped(_)) as err => err
  | Succeeded(ana_done) => Succeeded(AnaDone(ana_done));

let zcase_of_scrut_and_suffix =
    (u_gen: MetaVarGen.t, scrut: UHExp.t, suffix: list(UHExp.line))
    : (ZExp.zoperand, MetaVarGen.t) => {
  switch (scrut, suffix) {
  | ([ExpLine(OpSeq(_, S(EmptyHole(_), E)))], []) =>
    let zscrut = scrut |> ZExp.place_before;
    let (rule, u_gen) = u_gen |> UHExp.empty_rule;
    (ZExp.CaseZE(StandardErrStatus(NotInHole), zscrut, [rule]), u_gen);
  | (_, []) =>
    let (zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    (
      ZExp.CaseZR(StandardErrStatus(NotInHole), scrut, ([], zrule, [])),
      u_gen,
    );
  | ([ExpLine(OpSeq(_, S(EmptyHole(_), E)))], [_, ..._]) =>
    let zscrut = scrut |> ZExp.place_before;
    let (p_hole, u_gen) = u_gen |> UHPat.new_EmptyHole;
    let rule = UHExp.Rule(OpSeq.wrap(p_hole), suffix);
    (ZExp.CaseZE(StandardErrStatus(NotInHole), zscrut, [rule]), u_gen);
  | (_, [_, ..._]) =>
    let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
    let zrule = ZExp.RuleZP(ZOpSeq.wrap(zp_hole), suffix);
    (
      ZExp.CaseZR(StandardErrStatus(NotInHole), scrut, ([], zrule, [])),
      u_gen,
    );
  };
};

let mk_syn_text =
    (ctx: Contexts.t, u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t(syn_success) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TextShape.of_text(text)) {
  | InvalidTextShape(t) =>
    if (text |> StringUtil.is_empty) {
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      Succeeded(SynDone((ZExp.ZBlock.wrap(zhole), HTyp.Hole, u_gen)));
    } else {
      let (it, u_gen) = UHExp.new_InvalidText(u_gen, t);
      let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, it));
      Succeeded(SynDone((ze, HTyp.Hole, u_gen)));
    }
  | IntLit(n) =>
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, UHExp.intlit(n)));
    Succeeded(SynDone((ze, HTyp.Int, u_gen)));
  | FloatLit(f) =>
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, UHExp.floatlit(f)));
    Succeeded(SynDone((ze, HTyp.Float, u_gen)));
  | BoolLit(b) =>
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, UHExp.boollit(b)));
    Succeeded(SynDone((ze, HTyp.Bool, u_gen)));
  | ExpandingKeyword(k) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let var =
      UHExp.var(
        ~var_err=InVarHole(Keyword(k), u),
        k |> ExpandingKeyword.to_string,
      );
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, var));
    Succeeded(SynDone((ze, HTyp.Hole, u_gen)));
  | Underscore =>
    let (it, u_gen) = UHExp.new_InvalidText(u_gen, "_");
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, it));
    Succeeded(SynDone((ze, HTyp.Hole, u_gen)));
  | Var(x) =>
    switch (VarMap.lookup(ctx |> Contexts.gamma, x)) {
    | Some(ty) =>
      let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, UHExp.var(x)));
      Succeeded(SynDone((ze, ty, u_gen)));
    | None =>
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let var = UHExp.var(~var_err=InVarHole(Free, u), x);
      let new_ze = ZExp.ZBlock.wrap(CursorE(text_cursor, var));
      Succeeded(SynDone((new_ze, Hole, u_gen)));
    };
  };
};

let mk_ana_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t(_) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TextShape.of_text(text)) {
  | InvalidTextShape(t) =>
    if (text |> StringUtil.is_empty) {
      let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      Succeeded(AnaDone((ZExp.ZBlock.wrap(zhole), u_gen)));
    } else {
      let (it, u_gen) = UHExp.new_InvalidText(u_gen, t);
      let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, it));
      Succeeded(AnaDone((ze, u_gen)));
    }
  | ExpandingKeyword(k) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let var =
      UHExp.var(
        ~var_err=InVarHole(Keyword(k), u),
        k |> ExpandingKeyword.to_string,
      );
    let ze = ZExp.ZBlock.wrap(CursorE(text_cursor, var));
    Succeeded(AnaDone((ze, u_gen)));
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Underscore
  | Var(_) =>
    // TODO: review whether subsumption correctly applied
    switch (mk_syn_text(ctx, u_gen, caret_index, text)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded(SynExpands(r)) => Succeeded(AnaExpands(r))
    | Succeeded(SynDone((ze, ty', u_gen))) =>
      if (HTyp.consistent(ty, ty')) {
        Succeeded(AnaDone((ze, u_gen)));
      } else {
        let (ze, u_gen) = ze |> ZExp.mk_inconsistent(u_gen);
        Succeeded(AnaDone((ze, u_gen)));
      }
    }
  };
};

//TBD
let syn_insert_text = Action_common.syn_insert_text_(~mk_syn_text);
let ana_insert_text = Action_common.ana_insert_text_(~mk_ana_text);
let syn_backspace_text = Action_common.syn_backspace_text_(~mk_syn_text);
let ana_backspace_text = Action_common.ana_backspace_text_(~mk_ana_text);
let syn_delete_text = Action_common.syn_delete_text_(~mk_syn_text);
let ana_delete_text = Action_common.ana_delete_text_(~mk_ana_text);

let syn_split_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      sop: Action.operator_shape,
      text: string,
    )
    : ActionOutcome.t(syn_success) => {
  let (l, r) = text |> StringUtil.split_string(caret_index);
  switch (
    TextShape.of_text(l),
    operator_of_shape(sop),
    TextShape.of_text(r),
  ) {
  | (_, None, _) => Failed
  | (ExpandingKeyword(kw), Some(Space), rshape) =>
    let (subject, u_gen) = {
      let (operand, u_gen) = UHExp.text_operand(u_gen, rshape);
      (UHExp.Block.wrap(operand), u_gen);
    };
    Succeeded(
      switch (kw) {
      | Let => mk_SynExpandsToLet(~u_gen, ~def=subject, ())
      | Case => mk_SynExpandsToCase(~u_gen, ~scrut=subject, ())
      },
    );
  | (lshape, Some(op), rshape) =>
    let (loperand, u_gen) = UHExp.text_operand(u_gen, lshape);
    let (roperand, u_gen) = UHExp.text_operand(u_gen, rshape);
    let new_ze = {
      let zoperand = roperand |> ZExp.place_before_operand;
      let zopseq =
        ZExp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
      ZExp.ZBlock.wrap'(zopseq);
    };
    Succeeded(SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
  };
};
let ana_split_text =
    (
      ctx: Contexts.t,
      u_gen: MetaVarGen.t,
      caret_index: int,
      sop: Action.operator_shape,
      text: string,
      ty: HTyp.t,
    )
    : ActionOutcome.t(ana_success) => {
  let (l, r) = text |> StringUtil.split_string(caret_index);
  switch (
    TextShape.of_text(l),
    operator_of_shape(sop),
    TextShape.of_text(r),
  ) {
  | (_, None, _) => Failed
  | (ExpandingKeyword(kw), Some(Space), rshape) =>
    let (subject, u_gen) = {
      let (operand, u_gen) = UHExp.text_operand(u_gen, rshape);
      (UHExp.Block.wrap(operand), u_gen);
    };
    Succeeded(
      switch (kw) {
      | Let => mk_AnaExpandsToLet(~u_gen, ~def=subject, ())
      | Case => mk_AnaExpandsToCase(~u_gen, ~scrut=subject, ())
      },
    );
  | (lshape, Some(op), rshape) =>
    let (loperand, u_gen) = UHExp.text_operand(u_gen, lshape);
    let (roperand, u_gen) = UHExp.text_operand(u_gen, rshape);
    let new_ze = {
      let zoperand = roperand |> ZExp.place_before_operand;
      let zopseq =
        ZExp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
      ZExp.ZBlock.wrap'(zopseq);
    };
    Succeeded(AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)));
  };
};

let rec syn_move =
        (
          ctx: Contexts.t,
          a: Action.t,
          (ze: ZExp.t, ty: HTyp.t, u_gen: MetaVarGen.t),
        )
        : ActionOutcome.t(syn_success) =>
  switch (a) {
  /* Movement */
  | MoveTo(path) =>
    switch (CursorPath_Exp.follow(path, ze |> ZExp.erase)) {
    | None => Failed
    | Some(ze) => Succeeded(SynDone((ze, ty, u_gen)))
    }
  | MoveToPrevHole =>
    switch (CursorPath_Exp.prev_hole_steps_z(ze)) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Exp.of_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(path) => syn_move(ctx, MoveTo(path), (ze, ty, u_gen))
      }
    }
  | MoveToNextHole =>
    switch (CursorPath_Exp.next_hole_steps_z(ze)) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Exp.of_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(path) => syn_move(ctx, MoveTo(path), (ze, ty, u_gen))
      }
    }
  | MoveLeft =>
    switch (ZExp.move_cursor_left(ze)) {
    | None => CursorEscaped(Before)
    | Some(ze) => Succeeded(SynDone((ze, ty, u_gen)))
    }
  | MoveRight =>
    switch (ZExp.move_cursor_right(ze)) {
    | None => CursorEscaped(After)
    | Some(ze) => Succeeded(SynDone((ze, ty, u_gen)))
    }
  | Construct(_)
  | Delete
  | Backspace
  | UpdateApPalette(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  };

let rec ana_move =
        (
          ctx: Contexts.t,
          a: Action.t,
          (ze: ZExp.t, u_gen: MetaVarGen.t),
          ty: HTyp.t,
        )
        : ActionOutcome.t(ana_success) =>
  switch (a) {
  /* Movement */
  | MoveTo(path) =>
    switch (CursorPath_Exp.follow(path, ze |> ZExp.erase)) {
    | None => Failed
    | Some(ze) => Succeeded(AnaDone((ze, u_gen)))
    }
  | MoveToPrevHole =>
    switch (CursorPath_Exp.prev_hole_steps_z(ze)) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Exp.of_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(path) => ana_move(ctx, MoveTo(path), (ze, u_gen), ty)
      }
    }
  | MoveToNextHole =>
    switch (CursorPath_Exp.next_hole_steps_z(ze)) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Exp.of_steps(steps, ze |> ZExp.erase)) {
      | None => Failed
      | Some(path) => ana_move(ctx, MoveTo(path), (ze, u_gen), ty)
      }
    }
  | MoveLeft =>
    switch (ZExp.move_cursor_left(ze)) {
    | None => CursorEscaped(Before)
    | Some(ze) => Succeeded(AnaDone((ze, u_gen)))
    }
  | MoveRight =>
    switch (ZExp.move_cursor_right(ze)) {
    | None => CursorEscaped(After)
    | Some(ze) => Succeeded(AnaDone((ze, u_gen)))
    }
  | Construct(_)
  | Delete
  | Backspace
  | UpdateApPalette(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  };

let rec syn_perform =
        (
          ctx: Contexts.t,
          a: Action.t,
          (ze: ZExp.t, ty: HTyp.t, u_gen: MetaVarGen.t): Statics.edit_state,
        )
        : ActionOutcome.t(syn_done) => {
  switch (syn_perform_block(ctx, a, (ze, ty, u_gen))) {
  | (Failed | CursorEscaped(_)) as err => err
  | Succeeded(SynDone(syn_done)) => Succeeded(syn_done)
  | Succeeded(SynExpands({kw: Case, prefix, subject, suffix, u_gen})) =>
    let (zcase, u_gen) = zcase_of_scrut_and_suffix(u_gen, subject, suffix);
    let new_ze =
      (prefix, ZExp.ExpLineZ(zcase |> ZOpSeq.wrap), [])
      |> ZExp.prune_empty_hole_lines;
    Succeeded(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze));
  | Succeeded(SynExpands({kw: Let, prefix, subject, suffix, u_gen})) =>
    let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
    let zlet = ZExp.LetLineZP(ZOpSeq.wrap(zp_hole), subject);
    let new_ze = (prefix, zlet, suffix) |> ZExp.prune_empty_hole_lines;
    Succeeded(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze));
  };
}
and syn_perform_block =
    (
      ctx: Contexts.t,
      a: Action.t,
      (
        (prefix, zline, suffix) as zblock: ZExp.zblock,
        ty: HTyp.t,
        u_gen: MetaVarGen.t,
      ),
    )
    : ActionOutcome.t(syn_success) =>
  switch (a) {
  /* Movement */
  | MoveTo(_)
  | MoveToPrevHole
  | MoveToNextHole
  | MoveLeft
  | MoveRight => syn_move(ctx, a, (zblock, ty, u_gen))

  /* Backspace & Delete */
  // Handle 2 special cases for CommentLines
  // Case 1
  // e.g.,
  //  #  comment 1
  //  #| comment 2
  //    =( Backspace )=>
  //  #  comment 1|comment 2
  | Backspace when ZExp.is_begin_of_comment(zblock) =>
    switch (prefix |> ListUtil.split_last_opt) {
    | Some((new_prefix, pre_zline)) =>
      switch (pre_zline, zline) {
      | (CommentLine(pre_comment), CursorL(_, CommentLine(comment))) =>
        let new_zline =
          ZExp.CursorL(
            OnText(String.length(pre_comment)),
            CommentLine(pre_comment ++ comment),
          );
        let new_ze = (new_prefix, new_zline, suffix);
        Succeeded(SynDone((new_ze, ty, u_gen)));
      | _ => Failed
      }
    | _ => Failed
    }

  // Case 2
  // e.g.,
  //  # comment 1|
  //  # comment 2
  //    =( Delete )=>
  //  # comment 1|comment 2
  | Delete when ZExp.is_end_of_comment(zblock) =>
    switch (suffix, zline) {
    | (
        [CommentLine(post_comment), ...new_suffix],
        CursorL(_, CommentLine(comment)),
      ) =>
      let new_zline =
        ZExp.CursorL(
          OnText(String.length(comment)),
          CommentLine(comment ++ post_comment),
        );
      let new_ze = (prefix, new_zline, new_suffix);
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | _ => Failed
    }

  | Delete when ZExp.is_after_zline(zline) =>
    switch (zline |> ZExp.erase_zline, suffix) {
    | (_, []) => CursorEscaped(After)
    | (EmptyLine, [suffix_hd, ...new_suffix]) =>
      let new_zline = suffix_hd |> ZExp.place_before_line;
      let new_zblock = (prefix, new_zline, new_suffix);
      Succeeded(SynDone((new_zblock, ty, u_gen)));
    | (_, [EmptyLine, ...new_suffix])
    | (ExpLine(_), [ExpLine(OpSeq(_, S(EmptyHole(_), E))), ...new_suffix]) =>
      let new_ze = (prefix, zline, new_suffix);
      Succeeded(SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
    | _ =>
      syn_perform(ctx, MoveRight, (zblock, ty, u_gen)) |> wrap_in_SynDone
    }
  | Backspace when ZExp.is_before_zline(zline) =>
    switch (prefix |> ListUtil.split_last_opt, zline |> ZExp.erase_zline) {
    | (None, _) => CursorEscaped(Before)
    | (Some(([], EmptyLine)), EmptyLine) =>
      let new_ze = {
        let (_, new_zline, new_suffix) = suffix |> ZExp.place_before_block;
        ([UHExp.EmptyLine], new_zline, new_suffix);
      };
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (Some((new_prefix, EmptyLine)), _) =>
      let new_ze = (new_prefix, zline, suffix);
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (Some((new_prefix, prefix_hd)), EmptyLine) =>
      let new_zline = prefix_hd |> ZExp.place_after_line;
      let new_ze = (new_prefix, new_zline, suffix);
      Succeeded(SynDone((new_ze, ty, u_gen)));
    | (
        Some((new_prefix, ExpLine(_) as prefix_hd)),
        ExpLine(OpSeq(_, S(EmptyHole(_), E))),
      ) =>
      let new_zline = prefix_hd |> ZExp.place_after_line;
      let new_ze = (new_prefix, new_zline, suffix);
      Succeeded(SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
    | _ => syn_perform(ctx, MoveLeft, (zblock, ty, u_gen)) |> wrap_in_SynDone
    }

  /* No construction handled at block level */

  /* SwapUp and SwapDown is handled at block level */
  | SwapUp when ZExp.line_can_be_swapped(zline) =>
    switch (
      ListUtil.split_last_opt(prefix),
      zline |> ZExp.erase_zline,
      suffix,
    ) {
    | (None, _, _) => Failed
    | (Some((_, LetLine(_))), ExpLine(OpSeq(_, S(EmptyHole(_), E))), []) =>
      Failed
    /* handle the corner case when swapping the last line up where the second to last line is EmptyLine */
    | (Some((rest, EmptyLine)), _, []) =>
      let (new_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let new_zblock =
        (rest, zline, UHExp.Block.wrap(new_hole))
        |> ZExp.prune_empty_hole_lines;
      Succeeded(
        SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_zblock)),
      );
    | (Some((rest, last)), _, _) =>
      let new_zblock =
        (rest, zline, [last, ...suffix]) |> ZExp.prune_empty_hole_lines;
      Succeeded(
        SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_zblock)),
      );
    }
  | SwapDown when ZExp.line_can_be_swapped(zline) =>
    switch (suffix, zline) {
    | ([], _) => Failed
    /* avoid swap down for the Let line if it is second to last */
    | ([_], LetLineZP(_) | CursorL(_, LetLine(_))) => Failed
    /* handle corner case when the second to last line is an EmptyLine */
    | ([last], CursorL(_, EmptyLine)) =>
      let (new_hole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      let new_zblock =
        (prefix @ [last], ZExp.ExpLineZ(ZOpSeq.wrap(new_hole)), [])
        |> ZExp.prune_empty_hole_lines;
      Succeeded(
        SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_zblock)),
      );
    | ([hd, ...tl], _) =>
      let new_zblock =
        (prefix @ [hd], zline, tl) |> ZExp.prune_empty_hole_lines;
      Succeeded(
        SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_zblock)),
      );
    }

  /* Zipper */
  | _ =>
    switch (Statics_Exp.syn_lines(ctx, prefix)) {
    | None => Failed
    | Some(ctx_zline) =>
      switch (syn_perform_line(ctx_zline, a, (zline, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform(ctx, Action_common.escape(side), (zblock, ty, u_gen))
        |> wrap_in_SynDone
      | Succeeded(LineExpands(r)) =>
        Succeeded(
          SynExpands({
            ...r,
            prefix: prefix @ r.prefix,
            suffix: r.suffix @ suffix,
          }),
        )
      | Succeeded(
          LineDone((
            (inner_prefix, new_zline, inner_suffix) as zblock,
            ctx_suffix,
            u_gen,
          )),
        ) =>
        switch (suffix) {
        | [] =>
          switch (
            Statics_Exp.syn_block(ctx_zline, zblock |> ZExp.erase_zblock)
          ) {
          | None => Failed
          | Some(new_ty) =>
            let new_ze = (prefix @ inner_prefix, new_zline, inner_suffix);
            Succeeded(SynDone((new_ze, new_ty, u_gen)));
          }
        | [_, ..._] =>
          let (suffix, new_ty, u_gen) =
            Statics_Exp.syn_fix_holes_block(ctx_suffix, u_gen, suffix);
          let new_zblock =
            (prefix @ inner_prefix, new_zline, inner_suffix @ suffix)
            |> ZExp.prune_empty_hole_lines;
          Succeeded(SynDone((new_zblock, new_ty, u_gen)));
        }
      }
    }
  }
and syn_perform_line =
    (ctx: Contexts.t, a: Action.t, (zline: ZExp.zline, u_gen: MetaVarGen.t))
    : ActionOutcome.t(line_success) => {
  let mk_result = (u_gen, zlines): ActionOutcome.t(_) =>
    switch (Statics_Exp.syn_lines(ctx, ZExp.erase_zblock(zlines))) {
    | None => Failed
    | Some(ctx) => Succeeded(LineDone((zlines, ctx, u_gen)))
    };
  let fix_and_mk_result = (u_gen, zlines): ActionOutcome.t(_) => {
    let (zlines, ctx, u_gen) =
      Statics_Exp.syn_fix_holes_zlines(ctx, u_gen, zlines);
    Succeeded(LineDone((zlines, ctx, u_gen)));
  };
  let escape = (u_gen, side: Side.t) => {
    let move_cursor =
      switch (side) {
      | Before => ZExp.move_cursor_left_zline
      | After => ZExp.move_cursor_right_zline
      };
    switch (move_cursor(zline)) {
    | None => ActionOutcome.CursorEscaped(side)
    | Some(new_zline) => fix_and_mk_result(u_gen, ([], new_zline, []))
    };
  };

  switch (a, zline) {
  /* Invalid cursor positions */
  | (
      _,
      CursorL(OnDelim(_) | OnOp(_), EmptyLine) |
      CursorL(OnText(_) | OnOp(_), LetLine(_)) |
      CursorL(OnOp(_), CommentLine(_)) |
      CursorL(_, ExpLine(_)),
    ) =>
    Failed
  | (_, CursorL(cursor, line)) when !ZExp.is_valid_cursor_line(cursor, line) =>
    Failed

  /* Movement */
  | (MoveTo(path), _) =>
    zline
    |> ZExp.erase_zline
    |> CursorPath_Exp.follow_line(path)
    |> Option.fold(~none=ActionOutcome.Failed, ~some=zline =>
         mk_result(u_gen, ([], zline, []))
       )
  | (MoveToPrevHole, _) =>
    switch (CursorPath_Exp.prev_hole_steps_zline(zline)) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Exp.of_steps_line(
          steps,
          ~side=Before,
          zline |> ZExp.erase_zline,
        )
      ) {
      | None => Failed
      | Some(path) => syn_perform_line(ctx, MoveTo(path), (zline, u_gen))
      }
    }
  | (MoveToNextHole, _) =>
    switch (CursorPath_Exp.next_hole_steps_zline(zline)) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Exp.of_steps_line(
          steps,
          ~side=Before,
          zline |> ZExp.erase_zline,
        )
      ) {
      | None => Failed
      | Some(path) => syn_perform_line(ctx, MoveTo(path), (zline, u_gen))
      }
    }
  | (MoveLeft, _) =>
    zline
    |> ZExp.move_cursor_left_zline
    |> Option.fold(~none=ActionOutcome.CursorEscaped(Before), ~some=zline =>
         mk_result(u_gen, ([], zline, []))
       )
  | (MoveRight, _) =>
    zline
    |> ZExp.move_cursor_right_zline
    |> Option.fold(~none=ActionOutcome.CursorEscaped(After), ~some=zline =>
         mk_result(u_gen, ([], zline, []))
       )

  /* Backspace & Delete */

  /* Deletion of empty lines handled at block level */
  | (Backspace | Delete, CursorL(_, EmptyLine)) => Failed

  /* let x <|= 2   ==>   let x| = 2 */
  | (Backspace, CursorL(OnDelim(_, Before as side), _))
  /* let x =|> 2   ==>   let x = |2 */
  | (Delete, CursorL(OnDelim(_, After as side), _)) => escape(u_gen, side)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorL(OnDelim(k, Before), line)) =>
    let new_zline = ZExp.CursorL(OnDelim(k, After), line);
    syn_perform_line(ctx, Backspace, (new_zline, u_gen));

  | (Backspace, CursorL(OnDelim(k, After), LetLine(p, def))) =>
    if (k == 1) {
      /* let x :<| Int = 2   ==>   let x| = 2 */
      let zp = p |> ZPat.place_after;
      let new_zblock = ([], ZExp.LetLineZP(zp, def), []);
      fix_and_mk_result(u_gen, new_zblock);
    } else {
      let new_ze = k == 2 ? def |> ZExp.place_after : def |> ZExp.place_before; // changed 3 to 2?
      fix_and_mk_result(u_gen, new_ze);
    }

  /* #| some comment => | */
  | (Backspace, CursorL(OnDelim(_, After), CommentLine(_))) =>
    let new_zblock = ([], ZExp.CursorL(OnText(0), EmptyLine), []);
    mk_result(u_gen, new_zblock);

  /*
     # some comment|
     _
       =( Delete )=>
     # some comment
     |_
   */
  /* # some co|mment => # some co|ment */
  | (Delete, CursorL(OnText(j), CommentLine(comment))) =>
    if (j == String.length(comment)) {
      escape(u_gen, After);
    } else {
      let new_zblock = {
        let new_comment = comment |> StringUtil.delete(j);
        let new_line: UHExp.line = CommentLine(new_comment);
        ([], ZExp.CursorL(OnText(j), new_line), []);
      };
      mk_result(u_gen, new_zblock);
    }

  /*
     x
     |# some comment
       =( Backspace )=>
     x|
     # some comment
   */
  /* # some co|mment => # some c|mment */
  | (Backspace, CursorL(OnText(j), CommentLine(comment))) =>
    if (j == 0) {
      escape(u_gen, Before);
    } else {
      let new_zblock = {
        let new_comment = comment |> StringUtil.backspace(j);
        let new_line: UHExp.line = CommentLine(new_comment);
        ([], ZExp.CursorL(OnText(j - 1), new_line), []);
      };
      mk_result(u_gen, new_zblock);
    }

  /* Construction */

  | (Construct(SCommentLine), CursorL(_, EmptyLine)) =>
    let new_zblock = ([], ZExp.CursorL(OnText(0), CommentLine("")), []);
    mk_result(u_gen, new_zblock);

  // Another way to construct "SCommentLine" (To create multi-lines)
  //   # this is a mai|n comment
  //         =>
  //   # this is a mai
  //   # n comment
  | (Construct(SCommentLine), CursorL(OnText(loca), CommentLine(comment))) =>
    let com_bef = String.sub(comment, 0, loca);
    let com_aft = String.sub(comment, loca, String.length(comment) - loca);
    let new_zblock = (
      [UHExp.CommentLine(com_bef)],
      ZExp.CursorL(OnText(0), CommentLine(com_aft)),
      [],
    );
    mk_result(u_gen, new_zblock);

  | (Construct(SChar(s)), CursorL(OnText(j), CommentLine(comment))) =>
    let new_comment = comment |> StringUtil.insert(j, s);
    let new_zblock = {
      let new_line: UHExp.line = CommentLine(new_comment);
      ([], ZExp.CursorL(OnText(j + 1), new_line), []);
    };
    mk_result(u_gen, new_zblock);

  | (Construct(SLine), _) when zline |> ZExp.is_before_zline =>
    let new_zblock = ([UHExp.EmptyLine], zline, []);
    fix_and_mk_result(u_gen, new_zblock);
  | (Construct(SLine), _) when zline |> ZExp.is_after_zline =>
    /* If the current line is just a hole, leave it empty */
    let (prev_line, new_zline) =
      ZExp.zline_is_just_empty_hole(zline)
        ? (
          UHExp.EmptyLine,
          zline |> ZExp.erase_zline |> ZExp.place_before_line,
        )
        : (
          ZExp.erase_zline(zline),
          ZExp.place_before_line(
            ExpLine(OpSeq.wrap(UHExp.EmptyHole(u_gen))),
          ),
        );
    let new_zblock = ([prev_line], new_zline, []);
    fix_and_mk_result(u_gen, new_zblock);

  | (Construct(_), CursorL(_, EmptyLine)) =>
    let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
    let new_zline = ZExp.ExpLineZ(zhole |> ZOpSeq.wrap);
    syn_perform_line(ctx, a, (new_zline, u_gen));

  | (Construct(_) | UpdateApPalette(_), CursorL(OnDelim(_, side), _))
      when !ZExp.is_before_zline(zline) && !ZExp.is_after_zline(zline) =>
    let move_cursor =
      switch (side) {
      | Before => ZExp.move_cursor_left_zline
      | After => ZExp.move_cursor_right_zline
      };
    switch (zline |> move_cursor) {
    | None => Failed
    | Some(zline) => syn_perform_line(ctx, a, (zline, u_gen))
    };
  | (Construct(_) | UpdateApPalette(_), CursorL(_)) => Failed

  /* Invalid swap actions */
  | (SwapUp | SwapDown, CursorL(_) | LetLineZP(_)) => Failed
  | (SwapLeft, CursorL(_))
  | (SwapRight, CursorL(_)) => Failed

  /* Zipper */

  | (_, ExpLineZ(zopseq)) =>
    switch (Statics_Exp.syn_opseq(ctx, ZExp.erase_zopseq(zopseq))) {
    | None => Failed
    | Some(ty) =>
      switch (syn_perform_opseq(ctx, a, (zopseq, ty, u_gen))) {
      | (Failed | CursorEscaped(_)) as err => err
      | Succeeded(SynExpands(r)) => Succeeded(LineExpands(r))
      | Succeeded(SynDone((ze, _, u_gen))) =>
        Succeeded(LineDone((ze, ctx, u_gen)))
      }
    }

  | (_, LetLineZP(zp, def)) =>
    let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, ZPat.erase(zp), def);
    let ty_def =
      switch (Statics_Exp.syn(def_ctx, def)) {
      | None =>
        failwith("syn_perform_line: LetLineZP: definition doesn't synthesize")
      | Some(ty) => ty
      };
    switch (Action_Pat.ana_perform(ctx, u_gen, a, zp, ty_def)) {
    | Failed => Failed
    | CursorEscaped(side) => escape(u_gen, side)
    | Succeeded((new_zp, _, u_gen)) =>
      // NOTE: Need to fix holes since ana_perform may have created
      // holes if ty_def is inconsistent with pattern type
      let (new_zp, ty_p, _, u_gen) =
        Statics_Pat.syn_fix_holes_z(ctx, u_gen, new_zp);
      let p = ZPat.erase(new_zp);
      let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, p, def);
      let (new_def, u_gen) =
        Statics_Exp.ana_fix_holes(def_ctx, u_gen, def, ty_p);
      let new_zline = ZExp.LetLineZP(new_zp, new_def);
      let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, new_def);
      Succeeded(LineDone((([], new_zline, []), body_ctx, u_gen)));
    };

  | (_, LetLineZE(p, zdef)) =>
    switch (Statics_Pat.syn(ctx, p)) {
    | None => Failed
    | Some((ty_p, _)) =>
      let def = ZExp.erase(zdef);
      let def_ctx = Statics_Exp.extend_let_def_ctx(ctx, p, def);
      switch (ana_perform(def_ctx, a, (zdef, u_gen), ty_p)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(u_gen, side)
      | Succeeded((new_zdef, u_gen)) =>
        let new_zline = ZExp.LetLineZE(p, new_zdef);
        let new_def = ZExp.erase(new_zdef);
        let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, new_def);
        Succeeded(LineDone((([], new_zline, []), body_ctx, u_gen)));
      };
    }
  | (Init, _) => failwith("Init action should not be performed.")
  };
}
and syn_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action.t,
      (
        ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq,
        ty: HTyp.t,
        u_gen: MetaVarGen.t,
      ),
    )
    : ActionOutcome.t(syn_success) =>
  switch (a, zseq) {
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Invalid actions */
  | (UpdateApPalette(_), ZOperator(_)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(ctx, a, (ZExp.ZBlock.wrap'(zopseq), ty, u_gen))

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    syn_perform_opseq(ctx, Action_common.escape(side), (zopseq, ty, u_gen))

  /* Backspace "." from Float Op to get Int Op */
  /* ( +.<| ) ==> ( + ) */
  | (
      Backspace,
      ZOperator(
        (
          OnOp(After) as pos,
          (
            FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan |
            FEquals
          ) as oper,
        ),
        seq,
      ),
    ) =>
    let new_operator = {
      switch (oper) {
      | Operators_Exp.FPlus => Some(Operators_Exp.Plus)
      | Operators_Exp.FMinus => Some(Operators_Exp.Minus)
      | Operators_Exp.FTimes => Some(Operators_Exp.Times)
      | Operators_Exp.FDivide => Some(Operators_Exp.Divide)
      | Operators_Exp.FLessThan => Some(Operators_Exp.LessThan)
      | Operators_Exp.FGreaterThan => Some(Operators_Exp.GreaterThan)
      | Operators_Exp.FEquals => Some(Operators_Exp.Equals)
      | _ => None
      };
    };
    switch (new_operator) {
    | Some(op) =>
      let new_zoperator = (pos, op);
      let new_zseq = ZSeq.ZOperator(new_zoperator, seq);
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));
    | None => Failed
    };
  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    let new_ze =
      ZExp.ZBlock.wrap'(
        ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
      );
    syn_perform(ctx, Backspace, (new_ze, ty, u_gen)) |> wrap_in_SynDone;

  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, ZOperator((OnOp(After), _), surround)) =>
    let new_zseq = delete_operator(surround);
    Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      ZOperand(
        CursorE(_, EmptyHole(_)) as zhole,
        (A(Space, prefix_tl), suffix),
      ),
    )
      when ZExp.is_before_zoperand(zhole) =>
    let S(operand, new_prefix) = prefix_tl;
    let zoperand = operand |> ZExp.place_after_operand;
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
    Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      ZOperand(
        CursorE(_, EmptyHole(_)) as zhole,
        (prefix, A(Space, suffix_tl)),
      ),
    )
      when ZExp.is_after_zoperand(zhole) =>
    let S(operand, new_suffix) = suffix_tl;
    let zoperand = operand |> ZExp.place_before_operand;
    let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
    Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

  /* Construction */

  /* Making Float Operators from Int Operators */
  | (Construct(SChar(".")), ZOperator((pos, oper), seq)) =>
    let new_operator = {
      switch (oper) {
      | Operators_Exp.Plus => Some(Operators_Exp.FPlus)
      | Operators_Exp.Minus => Some(Operators_Exp.FMinus)
      | Operators_Exp.Times => Some(Operators_Exp.FTimes)
      | Operators_Exp.Divide => Some(Operators_Exp.FDivide)
      | Operators_Exp.LessThan => Some(Operators_Exp.FLessThan)
      | Operators_Exp.GreaterThan => Some(Operators_Exp.FGreaterThan)
      | Operators_Exp.Equals => Some(Operators_Exp.FEquals)
      | _ => None
      };
    };
    switch (new_operator) {
    | Some(op) =>
      let new_zoperator = (pos, op);
      let new_zseq = ZSeq.ZOperator(new_zoperator, seq);
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));
    | None => Failed
    };

  /* Space construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
      when ZExp.is_after_zoperator(zoperator) =>
    syn_perform_opseq(ctx, MoveRight, (zopseq, ty, u_gen))
  /* ...while other construction is applied after movement */
  | (Construct(_), ZOperator(zoperator, _)) =>
    let move_cursor =
      ZExp.is_before_zoperator(zoperator)
        ? ZExp.move_cursor_left_zopseq : ZExp.move_cursor_right_zopseq;
    switch (zopseq |> move_cursor) {
    | None => Failed
    | Some(zopseq) => syn_perform_opseq(ctx, a, (zopseq, ty, u_gen))
    };

  | (Construct(SLine), ZOperand(zoperand, (prefix, A(_) as suffix)))
      when zoperand |> ZExp.is_after_zoperand =>
    let (new_line, u_gen) = {
      let operand = zoperand |> ZExp.erase_zoperand;
      let seq = Seq.prefix_seq(prefix, S(operand, E));
      let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
      (UHExp.ExpLine(opseq), u_gen);
    };
    let (new_zline, ty, u_gen) = {
      let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let seq = Seq.seq_suffix(S(hole, E), suffix);
      let (opseq, ty, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
      (ZExp.ExpLineZ(opseq |> ZExp.place_before_opseq), ty, u_gen);
    };
    let new_zblock = ([new_line], new_zline, []);
    Succeeded(SynDone((new_zblock, ty, u_gen)));

  | (
      Construct(SLine),
      ZOperand(CursorE(_) as zoperand, (A(_) as prefix, suffix)),
    ) =>
    let (new_line, u_gen) = {
      let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let seq = Seq.prefix_seq(prefix, S(hole, E));
      let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
      (UHExp.ExpLine(opseq), u_gen);
    };
    let (new_zline, ty, u_gen) = {
      let zseq = ZSeq.ZOperand(zoperand, (E, suffix));
      let (zopseq, ty, u_gen) =
        // TODO clean up hack
        switch (mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)) {
        | (([], ExpLineZ(zopseq), []), ty, u_gen) => (zopseq, ty, u_gen)
        | _ => assert(false)
        };
      (ZExp.ExpLineZ(zopseq), ty, u_gen);
    };
    let new_zblock = ([new_line], new_zline, []);
    Succeeded(SynDone((new_zblock, ty, u_gen)));

  | (
      Construct(SOp(SSpace)),
      ZOperand(
        CursorE(_, Var(_, InVarHole(Keyword(k), _), _)) as zoperand,
        surround,
      ),
    )
      when ZExp.is_after_zoperand(zoperand) =>
    let (zhole, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zhole, surround));
    syn_perform_opseq(ctx, keyword_action(k), (zopseq, ty, u_gen));

  | (Construct(SOp(os)), ZOperand(zoperand, surround))
      when
        ZExp.is_before_zoperand(zoperand) || ZExp.is_after_zoperand(zoperand) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(operator) =>
      let construct_operator =
        ZExp.is_before_zoperand(zoperand)
          ? construct_operator_before_zoperand
          : construct_operator_after_zoperand;
      let (zseq, u_gen) =
        construct_operator(u_gen, operator, zoperand, surround);
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)));
    }
  /* Swap actions */
  | (SwapUp | SwapDown, ZOperator(_))
  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorE(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorE(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));
  | (SwapRight, ZOperand(CursorE(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorE(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, new_zseq)));

  /* Zipper */

  | (_, ZOperand(zoperand, (E, E))) =>
    syn_perform_operand(ctx, a, (zoperand, ty, u_gen))

  | (_, ZOperand(zoperand, (prefix, suffix) as surround)) =>
    let n = Seq.length_of_affix(prefix);
    switch (
      Statics_Exp.syn_nth_type_mode(ctx, n, zopseq |> ZExp.erase_zopseq)
    ) {
    | None => Failed
    | Some(Syn) =>
      switch (Statics_Exp.syn_operand(ctx, ZExp.erase_zoperand(zoperand))) {
      | None => Failed
      | Some(ty_zoperand) =>
        switch (syn_perform_operand(ctx, a, (zoperand, ty_zoperand, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform_opseq(
            ctx,
            Action_common.escape(side),
            (zopseq, ty, u_gen),
          )
        | Succeeded(SynExpands(r)) =>
          let (prefix_lines, u_gen) = lines_of_prefix(r.u_gen, prefix);
          let (new_subject, u_gen) =
            switch (r.subject, suffix) {
            | (
                [ExpLine(OpSeq(_, S(EmptyHole(_), E)))],
                A(Space, S(subject, suffix)),
              ) =>
              // if expanding keyword action is invoked on
              // empty hole followed by space, then drop
              // the empty hole and space
              resurround(u_gen, UHExp.Block.wrap(subject), (E, suffix))
            | _ => resurround(u_gen, r.subject, (E, suffix))
            };
          Succeeded(
            SynExpands({
              ...r,
              u_gen,
              prefix: prefix_lines,
              subject: new_subject,
              suffix: [],
            }),
          );
        | Succeeded(SynDone((ze, _, u_gen))) =>
          let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
          Succeeded(
            SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)),
          );
        }
      }
    | Some(Ana(ty_zoperand)) =>
      switch (ana_perform_operand(ctx, a, (zoperand, u_gen), ty_zoperand)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform_opseq(
          ctx,
          Action_common.escape(side),
          (zopseq, ty, u_gen),
        )
      | Succeeded(AnaExpands(r)) =>
        let (prefix_lines, u_gen) = lines_of_prefix(r.u_gen, prefix);
        let (new_subject, u_gen) =
          resurround(u_gen, r.subject, (E, suffix));
        Succeeded(
          SynExpands({
            ...r,
            u_gen,
            prefix: prefix_lines,
            subject: new_subject,
            suffix: [],
          }),
        );
      | Succeeded(AnaDone((ze, u_gen))) =>
        let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
        Succeeded(SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));
      }
    };
  | (Init, _) => failwith("Init action should not be performed.")
  }
and syn_perform_operand =
    (
      ctx: Contexts.t,
      a: Action.t,
      (zoperand: ZExp.zoperand, ty: HTyp.t, u_gen: MetaVarGen.t),
    )
    : ActionOutcome.t(syn_success) =>
  switch (a, zoperand) {
  /* Invalid cursor positions */
  | (
      _,
      CursorE(
        OnDelim(_) | OnOp(_),
        Var(_) | InvalidText(_, _) | IntLit(_) | FloatLit(_) | BoolLit(_) |
        ApPalette(_),
      ) |
      CursorE(
        OnText(_) | OnOp(_),
        EmptyHole(_) | ListNil(_) | Lam(_) | Inj(_) | Case(_) |
        Parenthesized(_) |
        ApPalette(_),
      ),
    ) =>
    Failed
  | (_, CursorE(cursor, operand))
      when !ZExp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  /* Invalid actions at expression level */
  | (Construct(SLine), CursorE(OnText(_), _))
  | (Construct(SList), CursorE(_)) => Failed
  | (Construct(SCloseSquareBracket), CursorE(_, _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    syn_move(ctx, a, (ZExp.ZBlock.wrap(zoperand), ty, u_gen))

  /* Backspace & Deletion */

  | (Backspace, _) when ZExp.is_before_zoperand(zoperand) =>
    CursorEscaped(Before)
  | (Delete, _) when ZExp.is_after_zoperand(zoperand) =>
    CursorEscaped(After)

  | (Backspace, CursorE(_, EmptyHole(_) as operand)) =>
    let ze = UHExp.Block.wrap(operand) |> ZExp.place_before;
    ze |> ZExp.is_after
      ? Succeeded(SynDone((ze, Hole, u_gen))) : CursorEscaped(Before);
  | (Delete, CursorE(_, EmptyHole(_) as operand)) =>
    let ze = UHExp.Block.wrap(operand) |> ZExp.place_after;
    ze |> ZExp.is_before
      ? Succeeded(SynDone((ze, Hole, u_gen))) : CursorEscaped(After);

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorE(OnDelim(_, Before), _)) =>
    syn_perform_operand(ctx, MoveLeft, (zoperand, ty, u_gen))
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorE(OnDelim(_, After), _)) =>
    syn_perform_operand(ctx, MoveRight, (zoperand, ty, u_gen))

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorE(OnDelim(k, Before), operand)) =>
    let new_zoperand = ZExp.CursorE(OnDelim(k, After), operand);
    syn_perform_operand(ctx, Backspace, (new_zoperand, ty, u_gen));

  | (Backspace, CursorE(OnDelim(_, After), ListNil(_))) =>
    let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
    let new_ze = ZExp.ZBlock.wrap(zhole);
    Succeeded(SynDone((new_ze, Hole, u_gen)));

  | (Delete, CursorE(OnText(j), InvalidText(_, t))) =>
    syn_delete_text(ctx, u_gen, j, t)
  | (Delete, CursorE(OnText(j), Var(_, _, x))) =>
    syn_delete_text(ctx, u_gen, j, x)
  | (Delete, CursorE(OnText(j), IntLit(_, n))) =>
    syn_delete_text(ctx, u_gen, j, n)
  | (Delete, CursorE(OnText(j), FloatLit(_, f))) =>
    syn_delete_text(ctx, u_gen, j, f)
  | (Delete, CursorE(OnText(j), BoolLit(_, b))) =>
    syn_delete_text(ctx, u_gen, j, string_of_bool(b))
  | (Backspace, CursorE(OnText(j), InvalidText(_, t))) =>
    syn_backspace_text(ctx, u_gen, j, t)
  | (Backspace, CursorE(OnText(j), Var(_, _, x))) =>
    syn_backspace_text(ctx, u_gen, j, x)
  | (Backspace, CursorE(OnText(j), IntLit(_, n))) =>
    syn_backspace_text(ctx, u_gen, j, n)
  | (Backspace, CursorE(OnText(j), FloatLit(_, f))) =>
    syn_backspace_text(ctx, u_gen, j, f)
  | (Backspace, CursorE(OnText(j), BoolLit(_, b))) =>
    syn_backspace_text(ctx, u_gen, j, string_of_bool(b))

  /* \x :<| Int . x + 1   ==>   \x| . x + 1 */
  | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, body))) =>
    let (p, body_ctx, u_gen) =
      Statics_Pat.ana_fix_holes(ctx, u_gen, p, Hole);
    let (body, body_ty, u_gen) =
      Statics_Exp.syn_fix_holes(body_ctx, u_gen, body);
    let new_ze =
      ZExp.ZBlock.wrap(LamZP(NotInHole, ZPat.place_after(p), body));
    ActionOutcome.Succeeded((new_ze, HTyp.Arrow(Hole, body_ty), u_gen))
    |> wrap_in_SynDone;

  | (
      Backspace,
      CursorE(
        OnDelim(k, After),
        (Lam(_, _, e) | Inj(_, _, e) | Case(_, e, _) | Parenthesized(e)) as operand,
      ),
    ) =>
    let place_cursor =
      switch (operand) {
      | Lam(_) =>
        switch (k) {
        | 0
        | 1 => ZExp.place_before
        | _two => ZExp.place_after
        }
      | _ =>
        switch (k) {
        | 0 => ZExp.place_before
        | _one => ZExp.place_after
        }
      };
    let new_ze = e |> place_cursor;
    Succeeded(SynDone(Statics_Exp.syn_fix_holes_z(ctx, u_gen, new_ze)));

  /* TODO consider deletion of type ascription on case */

  /* Construction */

  | (Construct(SCommentLine), _) =>
    if (ZExp.is_before_zoperand(zoperand)) {
      let operand = ZExp.erase_zoperand(zoperand);
      Succeeded(
        SynDone((
          (
            [],
            CursorL(OnText(0), CommentLine("")),
            [ExpLine(OpSeq.wrap(operand))],
          ),
          ty,
          u_gen,
        )),
      );
    } else {
      Failed;
    }

  // TODO consider relaxing guards and
  // merging with regular op construction
  | (Construct(SOp(sop)), CursorE(OnText(j), InvalidText(_, t)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, sop, t)
  | (Construct(SOp(sop)), CursorE(OnText(j), Var(_, _, x)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, sop, x)
  | (Construct(SOp(sop)), CursorE(OnText(j), BoolLit(_, b)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, sop, string_of_bool(b))
  | (Construct(SOp(sop)), CursorE(OnText(j), IntLit(_, n)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, sop, n)
  | (Construct(SOp(sop)), CursorE(OnText(j), FloatLit(_, f)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    syn_split_text(ctx, u_gen, j, sop, f)

  | (Construct(SCase), CursorE(_, operand)) =>
    Succeeded(
      mk_SynExpandsToCase(
        ~u_gen,
        ~scrut=UHExp.Block.wrap'(OpSeq.wrap(operand)),
        (),
      ),
    )
  | (Construct(SLet), CursorE(_, operand)) =>
    Succeeded(
      mk_SynExpandsToLet(
        ~u_gen,
        ~def=UHExp.Block.wrap'(OpSeq.wrap(operand)),
        (),
      ),
    )
  | (Construct(SAnn), CursorE(_)) => Failed
  | (Construct(SChar(s)), CursorE(_, EmptyHole(_))) =>
    syn_insert_text(ctx, u_gen, (0, s), "")
  | (Construct(SChar(s)), CursorE(OnText(j), InvalidText(_, t))) =>
    syn_insert_text(ctx, u_gen, (j, s), t)
  | (Construct(SChar(s)), CursorE(OnText(j), Var(_, _, x))) =>
    syn_insert_text(ctx, u_gen, (j, s), x)
  | (Construct(SChar(s)), CursorE(OnText(j), IntLit(_, n))) =>
    syn_insert_text(ctx, u_gen, (j, s), n)
  | (Construct(SChar(s)), CursorE(OnText(j), FloatLit(_, f))) =>
    syn_insert_text(ctx, u_gen, (j, s), f)
  | (Construct(SChar(s)), CursorE(OnText(j), BoolLit(_, b))) =>
    syn_insert_text(ctx, u_gen, (j, s), string_of_bool(b))
  | (Construct(SChar(_)), CursorE(_)) => Failed

  | (Construct(SListNil), CursorE(_, EmptyHole(_))) =>
    let new_ze =
      UHExp.listnil() |> ZExp.place_after_operand |> ZExp.ZBlock.wrap;
    let new_ty = HTyp.List(Hole);
    Succeeded(SynDone((new_ze, new_ty, u_gen)));
  | (Construct(SListNil), CursorE(_)) => Failed

  | (Construct(SParenthesized), CursorE(_)) =>
    let new_ze =
      ZExp.ZBlock.wrap(ParenthesizedZ(ZExp.ZBlock.wrap(zoperand)));
    Succeeded(SynDone((new_ze, ty, u_gen)));

  | (Construct(SInj(side)), CursorE(_)) =>
    let new_ze =
      ZExp.ZBlock.wrap(InjZ(NotInHole, side, ZExp.ZBlock.wrap(zoperand)));
    let new_ty =
      switch (side) {
      | L => HTyp.Sum(ty, Hole)
      | R => HTyp.Sum(Hole, ty)
      };
    Succeeded(SynDone((new_ze, new_ty, u_gen)));

  | (Construct(SLam), CursorE(_, operand)) =>
    let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
    let new_ze =
      ZExp.ZBlock.wrap(
        LamZP(NotInHole, ZOpSeq.wrap(zhole), UHExp.Block.wrap(operand)),
      );
    Succeeded(SynDone((new_ze, HTyp.Arrow(Hole, ty), u_gen)));

  | (Construct(SCloseParens), InjZ(err, side, zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      SynDone((
        ZExp.ZBlock.wrap(
          ZExp.CursorE(
            OnDelim(1, After),
            Inj(err, side, ZExp.erase_zblock(zblock)),
          ),
        ),
        ty,
        u_gen,
      )),
    )
  | (Construct(SCloseParens), ParenthesizedZ(zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      SynDone((
        ZExp.ZBlock.wrap(
          ZExp.CursorE(
            OnDelim(1, After),
            Parenthesized(ZExp.erase_zblock(zblock)),
          ),
        ),
        ty,
        u_gen,
      )),
    )

  | (
      Construct(SCloseParens),
      CursorE(
        OnDelim(1, Before),
        Parenthesized(_) as operand | Inj(_, _, _) as operand,
      ),
    ) =>
    Succeeded(
      SynDone((
        ZExp.ZBlock.wrap(ZExp.CursorE(OnDelim(1, After), operand)),
        ty,
        u_gen,
      )),
    )
  | (Construct(SCloseParens), CursorE(_, _)) => Failed

  | (Construct(SCloseBraces), LamZE(_, _, zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      SynDone((
        ZExp.ZBlock.wrap(
          ZExp.place_after_operand(ZExp.erase_zoperand(zoperand)),
        ),
        ty,
        u_gen,
      )),
    )
  | (
      Construct(SCloseBraces),
      CursorE(OnDelim(2, Before), Lam(_, _, _) as lam),
    ) =>
    Succeeded(
      SynDone((
        ZExp.ZBlock.wrap(ZExp.CursorE(OnDelim(2, After), lam)),
        ty,
        u_gen,
      )),
    )
  | (Construct(SCloseBraces), CursorE(_)) => Failed

  | (Construct(SApPalette(name)), CursorE(_, EmptyHole(_))) =>
    let palette_ctx = Contexts.palette_ctx(ctx);
    switch (PaletteCtx.lookup(palette_ctx, name)) {
    | None => Failed
    | Some(palette_defn) =>
      let init_model_cmd = palette_defn.init_model;
      let (init_model, init_splice_info, u_gen) =
        SpliceGenMonad.exec(init_model_cmd, SpliceInfo.empty, u_gen);
      switch (Statics_Exp.ana_splice_map(ctx, init_splice_info.splice_map)) {
      | None => Failed
      | Some(splice_ctx) =>
        let expansion_ty = palette_defn.expansion_ty;
        let expand = palette_defn.expand;
        let expansion = expand(init_model);
        switch (Statics_Exp.ana(splice_ctx, expansion, expansion_ty)) {
        | None => Failed
        | Some(_) =>
          Succeeded(
            SynDone((
              ZExp.(
                ZBlock.wrap(
                  place_before_operand(
                    ApPalette(NotInHole, name, init_model, init_splice_info),
                  ),
                )
              ),
              expansion_ty,
              u_gen,
            )),
          )
        };
      };
    };
  | (Construct(SApPalette(_)), CursorE(_)) => Failed
  /* TODO
     | (UpdateApPalette(_), CursorE(_, ApPalette(_, _name, _, _hole_data))) =>
        let (_, palette_ctx) = ctx;
        switch (PaletteCtx.lookup(palette_ctx, name)) {
        | Some(palette_defn) =>
          let (q, u_gen') = UHExp.HoleRefs.exec(monad, hole_data, u_gen);
          let (serialized_model, hole_data') = q;
          let expansion_ty = UHExp.PaletteDefinition.expansion_ty(palette_defn);
          let expansion =
            (UHExp.PaletteDefinition.to_exp(palette_defn))(serialized_model);
          let (_, hole_map') = hole_data';
          let expansion_ctx =
            UHExp.PaletteHoleData.extend_ctx_with_hole_map(ctx, hole_map');
          switch (Statics_common.ana(expansion_ctx, expansion, expansion_ty)) {
          | Some(_) =>
            Succeeded((
              CursorE(
                After,
                Tm(
                  NotInHole,
                  ApPalette(name, serialized_model, hole_data'),
                ),
              ),
              expansion_ty,
              u_gen,
            ))
          | None => Failed
          };
        | None => Failed
        }; */
  | (UpdateApPalette(_), CursorE(_)) => Failed

  | (Construct(SOp(SSpace)), CursorE(OnDelim(_, After), _))
      when !ZExp.is_after_zoperand(zoperand) =>
    syn_perform_operand(ctx, MoveRight, (zoperand, ty, u_gen))

  | (Construct(SOp(os)), CursorE(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(operator) =>
      let construct_operator =
        ZExp.is_before_zoperand(zoperand)
          ? construct_operator_before_zoperand
          : construct_operator_after_zoperand;
      let (zseq, u_gen) =
        construct_operator(u_gen, operator, zoperand, (E, E));
      Succeeded(SynDone(mk_and_syn_fix_ZOpSeq(ctx, u_gen, zseq)));
    }

  | (Construct(_), CursorE(OnDelim(_, side), _))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    switch (
      syn_perform(
        ctx,
        Action_common.escape(side),
        (ZExp.ZBlock.wrap(zoperand), ty, u_gen),
      )
    ) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded(new_edit_state) =>
      syn_perform(ctx, a, new_edit_state) |> wrap_in_SynDone
    }

  | (Construct(SLine), CursorE(_)) when ZExp.is_before_zoperand(zoperand) =>
    let new_ze = (
      [UHExp.EmptyLine],
      ZExp.ExpLineZ(ZOpSeq.wrap(zoperand)),
      [],
    );
    Succeeded(SynDone((new_ze, ty, u_gen)));
  | (Construct(SLine), CursorE(_)) when ZExp.is_after_zoperand(zoperand) =>
    let (new_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
    let new_zline =
      UHExp.ExpLine(OpSeq.wrap(new_hole)) |> ZExp.place_before_line;
    let new_ze = (
      [
        UHExp.ExpLine(zoperand |> ZExp.erase_zoperand |> OpSeq.wrap)
        |> UHExp.Line.prune_empty_hole,
      ],
      new_zline,
      [],
    );
    Succeeded(SynDone((new_ze, ty, u_gen)));
  | (Construct(SLine), CursorE(_)) => Failed

  /* Invalid Swap actions */
  | (SwapUp | SwapDown, CursorE(_) | LamZP(_)) => Failed
  | (SwapLeft | SwapRight, CursorE(_)) => Failed

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (syn_perform(ctx, a, (zbody, ty, u_gen))) {
    | Failed => Failed
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, ty, u_gen),
      )
    | Succeeded((new_zbody, new_ty, u_gen)) =>
      let new_ze = ZExp.ZBlock.wrap(ParenthesizedZ(new_zbody));
      Succeeded(SynDone((new_ze, new_ty, u_gen)));
    }
  | (_, LamZP(_, zp, body)) =>
    switch (Action_Pat.syn_perform(ctx, u_gen, a, zp)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      syn_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, ty, u_gen),
      )
    | Succeeded((zp, ty_p, body_ctx, u_gen)) =>
      let (body, ty_body, u_gen) =
        Statics_Exp.syn_fix_holes(body_ctx, u_gen, body);
      let new_ty = HTyp.Arrow(ty_p, ty_body);
      let new_ze = ZExp.ZBlock.wrap(LamZP(NotInHole, zp, body));
      Succeeded(SynDone((new_ze, new_ty, u_gen)));
    }
  | (_, LamZE(_, p, zbody)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty_p, ty_body)) =>
      switch (Statics_Pat.syn(ctx, p)) {
      | None => Failed
      | Some((_, body_ctx)) =>
        switch (syn_perform(body_ctx, a, (zbody, ty_body, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          syn_perform_operand(
            ctx,
            Action_common.escape(side),
            (zoperand, ty, u_gen),
          )
        | Succeeded((zbody, new_ty_body, u_gen)) =>
          let new_ty = HTyp.Arrow(ty_p, new_ty_body);
          let new_ze = ZExp.ZBlock.wrap(LamZE(NotInHole, p, zbody));
          Succeeded(SynDone((new_ze, new_ty, u_gen)));
        }
      }
    }

  | (_, InjZ(_, side, zbody)) =>
    switch (ty) {
    | Sum(ty1, ty2) =>
      let ty_side = InjSide.pick(side, ty1, ty2);
      switch (syn_perform(ctx, a, (zbody, ty_side, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, ty, u_gen),
        )
      | Succeeded((zbody, ty_side', u_gen)) =>
        let new_ty =
          switch (side) {
          | L => HTyp.Sum(ty_side', ty2)
          | R => HTyp.Sum(ty1, ty_side')
          };
        let new_ze = ZExp.ZBlock.wrap(InjZ(NotInHole, side, zbody));
        Succeeded(SynDone((new_ze, new_ty, u_gen)));
      };
    | _ => Failed /* should never happen */
    }
  | (_, ApPaletteZ(_, _name, _serialized_model, _z_hole_data)) => Failed
  /* TODO let (next_lbl, z_nat_map) = z_hole_data;
     let (rest_map, z_data) = z_nat_map;
     let (cell_lbl, cell_data) = z_data;
     let (cell_ty, cell_ze) = cell_data;
     switch (ana_perform_operand(ctx, a, (cell_ze, u_gen), cell_ty)) {
     | Failed => Failed
          | Succeeded((cell_ze', u_gen')) =>
       let z_hole_data' = (
         next_lbl,
         (rest_map, (cell_lbl, (cell_ty, cell_ze'))),
       );
       Succeeded((
         ApPaletteZ(NotInHole, name, serialized_model, z_hole_data'),
         ty,
         u_gen',
       ));
     }; */
  | (_, CaseZE(_, zscrut, rules)) =>
    switch (Statics_Exp.syn(ctx, ZExp.erase(zscrut))) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform(ctx, a, (zscrut, ty1, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, ty, u_gen),
        )
      | Succeeded((zscrut, ty1, u_gen)) =>
        let (rules, u_gen, rule_types, common_type) =
          Statics_Exp.syn_fix_holes_rules(ctx, u_gen, rules, ty1);
        switch (common_type) {
        | None =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let new_ze =
            ZExp.ZBlock.wrap(
              CaseZE(InconsistentBranches(rule_types, u), zscrut, rules),
            );
          Succeeded(SynDone((new_ze, HTyp.Hole, u_gen)));
        | Some(ty) =>
          let new_ze =
            ZExp.ZBlock.wrap(
              CaseZE(StandardErrStatus(NotInHole), zscrut, rules),
            );
          Succeeded(SynDone((new_ze, ty, u_gen)));
        };
      }
    }
  | (_, CaseZR(_, scrut, zrules)) =>
    switch (Statics_Exp.syn(ctx, scrut)) {
    | None => Failed
    | Some(pat_ty) =>
      switch (syn_perform_rules(ctx, a, (zrules, u_gen), pat_ty)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        syn_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, ty, u_gen),
        )
      | Succeeded((new_zrules, u_gen)) =>
        let (new_zrules, rule_types, common_type, u_gen) =
          Statics_Exp.syn_fix_holes_zrules(ctx, u_gen, new_zrules, pat_ty);
        switch (common_type) {
        | None =>
          let (u, u_gen) = MetaVarGen.next(u_gen);
          let new_ze =
            ZExp.ZBlock.wrap(
              CaseZR(InconsistentBranches(rule_types, u), scrut, new_zrules),
            );
          Succeeded(SynDone((new_ze, HTyp.Hole, u_gen)));
        | Some(ty) =>
          let new_ze =
            ZExp.ZBlock.wrap(
              CaseZR(StandardErrStatus(NotInHole), scrut, new_zrules),
            );
          Succeeded(SynDone((new_ze, ty, u_gen)));
        };
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and syn_perform_rules =
    (
      ctx: Contexts.t,
      a: Action.t,
      ((prefix, zrule, suffix) as zrules: ZExp.zrules, u_gen: MetaVarGen.t),
      pat_ty: HTyp.t,
    )
    : ActionOutcome.t((ZExp.zrules, MetaVarGen.t)) => {
  let escape = (side: Side.t) => {
    let move_cursor =
      switch (side) {
      | Before => ZExp.move_cursor_left_zrules
      | After => ZExp.move_cursor_right_zrules
      };
    switch (move_cursor(zrules)) {
    | None => ActionOutcome.CursorEscaped(side)
    | Some(new_zrules) => Succeeded((new_zrules, u_gen))
    };
  };

  switch (a, zrule) {
  /* Invalid cursor positions */
  | (_, CursorR(OnText(_) | OnOp(_), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    failwith("unimplemented")

  /* Backspace & Delete */

  | (Backspace, CursorR(OnDelim(_, Before as side), _))
  | (Delete, CursorR(OnDelim(_, After as side), _)) => escape(side)

  // Delete before delim == Backspace after delim
  | (Delete, CursorR(OnDelim(k, Before), rule)) =>
    let new_zrules =
      zrules |> ZList.replace_z(ZExp.CursorR(OnDelim(k, After), rule));
    syn_perform_rules(ctx, Backspace, (new_zrules, u_gen), pat_ty);
  | (Backspace, CursorR(OnDelim(_, After), _)) =>
    switch (prefix |> ListUtil.split_last_opt, suffix) {
    | (None, []) =>
      let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      let new_zrules = ([], new_zrule, []);
      Succeeded((new_zrules, u_gen));
    | (_, [suffix_hd, ...new_suffix]) =>
      let new_zrule = suffix_hd |> ZExp.place_before_rule;
      let new_zrules = (prefix, new_zrule, new_suffix);
      Succeeded((new_zrules, u_gen));
    | (Some((new_prefix, prefix_last)), _) =>
      let new_zrule = prefix_last |> ZExp.place_after_rule;
      let new_zrules = (new_prefix, new_zrule, suffix);
      Succeeded((new_zrules, u_gen));
    }

  /* Construction */

  | (Construct(SOp(SSpace)), CursorR(OnDelim(_, After), _))
      when !ZExp.is_after_zrule(zrule) =>
    escape(After)

  | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_before =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix,
      new_zrule,
      [zrule |> ZExp.erase_zrule, ...suffix],
    );
    Succeeded((new_zrules, u_gen));
  | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_after =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix @ [zrule |> ZExp.erase_zrule],
      new_zrule,
      suffix,
    );
    Succeeded((new_zrules, u_gen));
  | (Construct(SLine), RuleZE(_, zclause)) when zclause |> ZExp.is_after =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix @ [zrule |> ZExp.erase_zrule],
      new_zrule,
      suffix,
    );
    Succeeded((new_zrules, u_gen));

  | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_, side), _))
      when !ZExp.is_before_zrule(zrule) && !ZExp.is_after_zrule(zrule) =>
    switch (escape(side)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zrules, u_gen)) =>
      syn_perform_rules(ctx, a, (zrules, u_gen), pat_ty)
    }
  | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_), _)) => Failed

  /* Invalid swap actions */
  | (SwapLeft | SwapRight, CursorR(_)) => Failed

  /* SwapUp and SwapDown actions */
  | (SwapUp, CursorR(_) | RuleZP(_)) =>
    switch (ListUtil.split_last_opt(prefix)) {
    | None => Failed
    | Some((rest, last)) =>
      let new_zrules = (rest, zrule, [last, ...suffix]);
      Succeeded((new_zrules, u_gen));
    }
  | (SwapDown, CursorR(_) | RuleZP(_)) =>
    switch (suffix) {
    | [] => Failed
    | [hd, ...tl] =>
      let new_zrules = (prefix @ [hd], zrule, tl);
      Succeeded((new_zrules, u_gen));
    }

  /* Zipper */
  | (_, RuleZP(zp, clause)) =>
    switch (Action_Pat.ana_perform(ctx, u_gen, a, zp, pat_ty)) {
    | Failed => Failed
    | CursorEscaped(side) => escape(side)
    | Succeeded((new_zp, ctx, u_gen)) =>
      let (clause, _, u_gen) = Statics_Exp.syn_fix_holes(ctx, u_gen, clause);
      let new_zrules =
        zrules |> ZList.replace_z(ZExp.RuleZP(new_zp, clause));
      Succeeded((new_zrules, u_gen));
    }

  | (_, RuleZE(p, zclause)) =>
    switch (Statics_Pat.ana(ctx, p, pat_ty)) {
    | None => Failed
    | Some(ctx) =>
      switch (syn_perform(ctx, a, (zclause, pat_ty, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) => escape(side)
      | Succeeded((new_zclause, _, u_gen)) =>
        let new_zrules =
          zrules |> ZList.replace_z(ZExp.RuleZE(p, new_zclause));
        Succeeded((new_zrules, u_gen));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  };
}
and ana_perform_rules =
    (
      ctx: Contexts.t,
      a: Action.t,
      ((prefix, zrule, suffix) as zrules: ZExp.zrules, u_gen: MetaVarGen.t),
      pat_ty: HTyp.t,
      clause_ty: HTyp.t,
    )
    : ActionOutcome.t((ZExp.zrules, MetaVarGen.t)) => {
  let escape = (side: Side.t) => {
    let move_cursor =
      switch (side) {
      | Before => ZExp.move_cursor_left_zrules
      | After => ZExp.move_cursor_right_zrules
      };
    switch (move_cursor(zrules)) {
    | None => ActionOutcome.CursorEscaped(side)
    | Some(new_zrules) => Succeeded((new_zrules, u_gen))
    };
  };

  switch (a, zrule) {
  /* Invalid cursor positions */
  | (_, CursorR(OnText(_) | OnOp(_), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    failwith("unimplemented")

  /* Backspace & Delete */

  | (Backspace, CursorR(OnDelim(_, Before as side), _))
  | (Delete, CursorR(OnDelim(_, After as side), _)) => escape(side)

  // Delete before delim == Backspace after delim
  | (Delete, CursorR(OnDelim(k, Before), rule)) =>
    let new_zrules =
      zrules |> ZList.replace_z(ZExp.CursorR(OnDelim(k, After), rule));
    ana_perform_rules(
      ctx,
      Backspace,
      (new_zrules, u_gen),
      pat_ty,
      clause_ty,
    );
  | (Backspace, CursorR(OnDelim(_, After), _)) =>
    switch (prefix |> ListUtil.split_last_opt, suffix) {
    | (None, []) =>
      let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
      let new_zrules = ([], new_zrule, []);
      Succeeded((new_zrules, u_gen));
    | (_, [suffix_hd, ...new_suffix]) =>
      let new_zrule = suffix_hd |> ZExp.place_before_rule;
      let new_zrules = (prefix, new_zrule, new_suffix);
      Succeeded((new_zrules, u_gen));
    | (Some((new_prefix, prefix_last)), _) =>
      let new_zrule = prefix_last |> ZExp.place_after_rule;
      let new_zrules = (new_prefix, new_zrule, suffix);
      Succeeded((new_zrules, u_gen));
    }

  /* Construction */

  | (Construct(SOp(SSpace)), CursorR(OnDelim(_, After), _))
      when !ZExp.is_after_zrule(zrule) =>
    escape(After)

  | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_before =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix,
      new_zrule,
      [zrule |> ZExp.erase_zrule, ...suffix],
    );
    Succeeded((new_zrules, u_gen));
  | (Construct(SLine), RuleZP(zp, _)) when zp |> ZPat.is_after =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix @ [zrule |> ZExp.erase_zrule],
      new_zrule,
      suffix,
    );
    Succeeded((new_zrules, u_gen));
  | (Construct(SLine), RuleZE(_, zclause)) when zclause |> ZExp.is_after =>
    let (new_zrule, u_gen) = u_gen |> ZExp.empty_zrule;
    let new_zrules = (
      prefix @ [zrule |> ZExp.erase_zrule],
      new_zrule,
      suffix,
    );
    Succeeded((new_zrules, u_gen));

  | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_, side), _))
      when !ZExp.is_before_zrule(zrule) && !ZExp.is_after_zrule(zrule) =>
    switch (escape(side)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zrules, u_gen)) =>
      ana_perform_rules(ctx, a, (zrules, u_gen), pat_ty, clause_ty)
    }
  | (Construct(_) | UpdateApPalette(_), CursorR(OnDelim(_), _)) => Failed

  /* Invalid swap actions */
  | (SwapLeft | SwapRight, CursorR(_)) => Failed

  /* SwapUp and SwapDown actions */
  | (SwapUp, CursorR(_) | RuleZP(_)) =>
    switch (ListUtil.split_last_opt(prefix)) {
    | None => Failed
    | Some((rest, last)) =>
      let new_zrules = (rest, zrule, [last, ...suffix]);
      Succeeded((new_zrules, u_gen));
    }
  | (SwapDown, CursorR(_) | RuleZP(_)) =>
    switch (suffix) {
    | [] => Failed
    | [hd, ...tl] =>
      let new_zrules = (prefix @ [hd], zrule, tl);
      Succeeded((new_zrules, u_gen));
    }

  /* Zipper */
  | (_, RuleZP(zp, clause)) =>
    switch (Action_Pat.ana_perform(ctx, u_gen, a, zp, pat_ty)) {
    | Failed => Failed
    | CursorEscaped(side) => escape(side)
    | Succeeded((new_zp, ctx, u_gen)) =>
      let (clause, u_gen) =
        Statics_Exp.ana_fix_holes(ctx, u_gen, clause, clause_ty);
      let new_zrules =
        zrules |> ZList.replace_z(ZExp.RuleZP(new_zp, clause));
      Succeeded((new_zrules, u_gen));
    }

  | (_, RuleZE(p, zclause)) =>
    switch (Statics_Pat.ana(ctx, p, pat_ty)) {
    | None => Failed
    | Some(ctx) =>
      switch (ana_perform(ctx, a, (zclause, u_gen), clause_ty)) {
      | Failed => Failed
      | CursorEscaped(side) => escape(side)
      | Succeeded((new_zclause, u_gen)) =>
        let new_zrules =
          zrules |> ZList.replace_z(ZExp.RuleZE(p, new_zclause));
        Succeeded((new_zrules, u_gen));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  };
}
and ana_perform =
    (
      ctx: Contexts.t,
      a: Action.t,
      (ze, u_gen): (ZExp.t, MetaVarGen.t),
      ty: HTyp.t,
    )
    : ActionOutcome.t((ZExp.t, MetaVarGen.t)) =>
  switch (ana_perform_block(ctx, a, (ze, u_gen), ty)) {
  | (Failed | CursorEscaped(_)) as err => err
  | Succeeded(AnaDone(ana_done)) => Succeeded(ana_done)
  | Succeeded(AnaExpands({kw: Case, prefix, subject, suffix, u_gen})) =>
    let (zcase, u_gen) = zcase_of_scrut_and_suffix(u_gen, subject, suffix);
    let new_zblock =
      (prefix, ZExp.ExpLineZ(zcase |> ZOpSeq.wrap), [])
      |> ZExp.prune_empty_hole_lines;
    Succeeded(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty));
  | Succeeded(AnaExpands({kw: Let, prefix, subject, suffix, u_gen})) =>
    let (zp_hole, u_gen) = u_gen |> ZPat.new_EmptyHole;
    let zlet = ZExp.LetLineZP(ZOpSeq.wrap(zp_hole), subject);
    let new_zblock = (prefix, zlet, suffix) |> ZExp.prune_empty_hole_lines;
    Succeeded(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty));
  }
and ana_perform_block =
    (
      ctx: Contexts.t,
      a: Action.t,
      ((prefix, zline, suffix) as zblock: ZExp.zblock, u_gen: MetaVarGen.t),
      ty: HTyp.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (a, zline) {
  /* Movement */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    ana_move(ctx, a, (zblock, u_gen), ty)

  /* Backspace & Delete */
  | (Backspace, _) when ZExp.is_begin_of_comment(zblock) =>
    switch (prefix |> ListUtil.split_last_opt) {
    | Some((new_prefix, pre_zline)) =>
      switch (pre_zline, zline) {
      | (CommentLine(pre_comment), CursorL(_, CommentLine(comment))) =>
        let new_zline =
          ZExp.CursorL(
            OnText(String.length(pre_comment)),
            CommentLine(pre_comment ++ comment),
          );
        let new_ze = (new_prefix, new_zline, suffix);
        Succeeded(AnaDone((new_ze, u_gen)));
      | _ => Failed
      }
    | _ => Failed
    }

  | (Delete, _) when ZExp.is_end_of_comment(zblock) =>
    switch (suffix, zline) {
    | (
        [CommentLine(post_comment), ...new_suffix],
        CursorL(_, CommentLine(comment)),
      ) =>
      let new_zline =
        ZExp.CursorL(
          OnText(String.length(comment)),
          CommentLine(comment ++ post_comment),
        );
      let new_ze = (prefix, new_zline, new_suffix);
      Succeeded(AnaDone((new_ze, u_gen)));
    | _ => Failed
    }

  | (Delete, _) when ZExp.is_after_zline(zline) =>
    switch (zline |> ZExp.erase_zline, suffix) {
    | (_, []) => CursorEscaped(After)
    | (EmptyLine, [suffix_hd, ...new_suffix]) =>
      let new_zline = suffix_hd |> ZExp.place_before_line;
      let new_ze = (prefix, new_zline, new_suffix);
      Succeeded(AnaDone((new_ze, u_gen)));
    | (_, [EmptyLine, ...new_suffix])
    | (ExpLine(_), [ExpLine(OpSeq(_, S(EmptyHole(_), E))), ...new_suffix]) =>
      let new_ze = (prefix, zline, new_suffix);
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
      );
    | _ =>
      ana_perform(ctx, MoveRight, (zblock, u_gen), ty) |> wrap_in_AnaDone
    }
  | (Backspace, _) when ZExp.is_before_zline(zline) =>
    switch (prefix |> ListUtil.split_last_opt, zline |> ZExp.erase_zline) {
    | (None, _) => CursorEscaped(Before)
    | (Some(([], EmptyLine)), EmptyLine) =>
      let new_ze = {
        let (_, new_zline, new_suffix) = suffix |> ZExp.place_before_block;
        ([UHExp.EmptyLine], new_zline, new_suffix);
      };
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Some((new_prefix, EmptyLine)), _) =>
      let new_ze = (new_prefix, zline, suffix);
      Succeeded(AnaDone((new_ze, u_gen)));
    | (Some((new_prefix, prefix_hd)), EmptyLine) =>
      let new_zline = prefix_hd |> ZExp.place_after_line;
      let new_ze = (new_prefix, new_zline, suffix);
      Succeeded(AnaDone((new_ze, u_gen)));
    | (
        Some((new_prefix, ExpLine(_) as prefix_hd)),
        ExpLine(OpSeq(_, S(EmptyHole(_), E))),
      ) =>
      let new_zline = prefix_hd |> ZExp.place_after_line;
      let new_ze = (new_prefix, new_zline, suffix);
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
      );
    | _ => ana_perform(ctx, MoveLeft, (zblock, u_gen), ty) |> wrap_in_AnaDone
    }

  /* No construction handled at block level */

  /* SwapUp and SwapDown is handled at block level */
  | (SwapUp, _) when ZExp.line_can_be_swapped(zline) =>
    switch (
      ListUtil.split_last_opt(prefix),
      zline |> ZExp.erase_zline,
      suffix,
    ) {
    | (None, _, _) => Failed
    | (Some((_, LetLine(_))), ExpLine(OpSeq(_, S(EmptyHole(_), E))), []) =>
      Failed
    /* handle the corner case when swapping the last line up where the second to last line is EmptyLine */
    | (Some((rest, EmptyLine)), _, []) =>
      let (new_hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let new_zblock =
        (rest, zline, UHExp.Block.wrap(new_hole))
        |> ZExp.prune_empty_hole_lines;
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty)),
      );
    | (Some((rest, last)), _, _) =>
      let new_zblock =
        (rest, zline, [last, ...suffix]) |> ZExp.prune_empty_hole_lines;
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty)),
      );
    }
  | (SwapDown, _) when ZExp.line_can_be_swapped(zline) =>
    switch (suffix, zline) {
    | ([], _) => Failed
    /* avoid swap down for the Let line if it is second to last */
    | ([_], LetLineZP(_) | CursorL(_, LetLine(_))) => Failed
    /* handle corner case when the second to last line is an EmptyLine */
    | ([last], CursorL(_, EmptyLine)) =>
      let (new_hole, u_gen) = u_gen |> ZExp.new_EmptyHole;
      let new_zblock =
        (prefix @ [last], ZExp.ExpLineZ(ZOpSeq.wrap(new_hole)), [])
        |> ZExp.prune_empty_hole_lines;
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty)),
      );
    | ([hd, ...tl], _) =>
      let new_zblock =
        (prefix @ [hd], zline, tl) |> ZExp.prune_empty_hole_lines;
      Succeeded(
        AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_zblock, ty)),
      );
    }
  /* Zipper */
  | _ =>
    switch (Statics_Exp.syn_lines(ctx, prefix)) {
    | None => Failed
    | Some(ctx_zline) =>
      switch (suffix) {
      | [] =>
        switch (zline) {
        | CursorL(_)
        | LetLineZP(_)
        | LetLineZE(_) => Failed
        | ExpLineZ(zopseq) =>
          switch (ana_perform_opseq(ctx_zline, a, (zopseq, u_gen), ty)) {
          | Failed => Failed
          | CursorEscaped(side) =>
            ana_perform_block(
              ctx,
              Action_common.escape(side),
              (zblock, u_gen),
              ty,
            )
          | Succeeded(AnaExpands(r)) =>
            Succeeded(
              AnaExpands({
                ...r,
                prefix: prefix @ r.prefix,
                suffix: r.suffix @ suffix,
              }),
            )
          | Succeeded(AnaDone(((inner_prefix, zline, suffix), u_gen))) =>
            let new_ze = (prefix @ inner_prefix, zline, suffix);
            Succeeded(AnaDone((new_ze, u_gen)));
          }
        }
      | [_, ..._] =>
        switch (syn_perform_line(ctx_zline, a, (zline, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform_block(
            ctx,
            Action_common.escape(side),
            (zblock, u_gen),
            ty,
          )
        | Succeeded(LineExpands(r)) =>
          Succeeded(
            AnaExpands({
              ...r,
              prefix: prefix @ r.prefix,
              suffix: r.suffix @ suffix,
            }),
          )
        | Succeeded(
            LineDone((
              (inner_prefix, new_zline, inner_suffix),
              ctx_suffix,
              u_gen,
            )),
          ) =>
          let (suffix, u_gen) =
            Statics_Exp.ana_fix_holes_block(ctx_suffix, u_gen, suffix, ty);
          let new_zblock =
            (prefix @ inner_prefix, new_zline, inner_suffix @ suffix)
            |> ZExp.prune_empty_hole_lines;
          Succeeded(AnaDone((new_zblock, u_gen)));
        }
      }
    }
  }
and ana_perform_opseq =
    (
      ctx: Contexts.t,
      a: Action.t,
      (ZOpSeq(skel, zseq) as zopseq: ZExp.zopseq, u_gen: MetaVarGen.t),
      ty: HTyp.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (a, zseq) {
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Invalid actions */
  | (UpdateApPalette(_), ZOperator(_)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    ana_move(ctx, a, (ZExp.ZBlock.wrap'(zopseq), u_gen), ty)

  /* Deletion */
  /* Backspace "." from Float Op to get Int Op */
  /* ( +.<| ) ==> ( + ) */
  | (
      Backspace,
      ZOperator(
        (
          OnOp(After) as pos,
          (
            FPlus | FMinus | FTimes | FDivide | FLessThan | FGreaterThan |
            FEquals
          ) as oper,
        ),
        seq,
      ),
    ) =>
    let new_operator = {
      switch (oper) {
      | Operators_Exp.FPlus => Some(Operators_Exp.Plus)
      | Operators_Exp.FMinus => Some(Operators_Exp.Minus)
      | Operators_Exp.FTimes => Some(Operators_Exp.Times)
      | Operators_Exp.FDivide => Some(Operators_Exp.Divide)
      | Operators_Exp.FLessThan => Some(Operators_Exp.LessThan)
      | Operators_Exp.FGreaterThan => Some(Operators_Exp.GreaterThan)
      | Operators_Exp.FEquals => Some(Operators_Exp.Equals)
      | _ => None
      };
    };
    switch (new_operator) {
    | Some(op) =>
      let new_zoperator = (pos, op);
      let new_zseq = ZSeq.ZOperator(new_zoperator, seq);
      Succeeded(AnaDone(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty)));
    | None => Failed
    };

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    ana_perform_opseq(ctx, Action_common.escape(side), (zopseq, u_gen), ty)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    let new_zopseq =
      ZOpSeq.ZOpSeq(
        skel,
        ZOperator((CursorPosition.OnOp(After), op), surround),
      );
    ana_perform_opseq(ctx, Backspace, (new_zopseq, u_gen), ty);

  /* ... + [k-1] +<| [k] + ... */
  | (Backspace, ZOperator((OnOp(After), _), surround)) =>
    let new_zseq = delete_operator(surround);
    ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
    |> wrap_in_AnaDone;

  /* ... + [k-1]  <|_ + [k+1] + ...  ==>   ... + [k-1]| + [k+1] + ... */
  | (
      Backspace,
      ZOperand(
        CursorE(_, EmptyHole(_)) as zhole,
        (A(Space, prefix_tl), suffix),
      ),
    )
      when ZExp.is_before_zoperand(zhole) =>
    let S(operand, new_prefix) = prefix_tl;
    let zoperand = operand |> ZExp.place_after_operand;
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
    ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
    |> wrap_in_AnaDone;

  /* ... + [k-1] + _|>  [k+1] + ...  ==>   ... + [k-1] + |[k+1] + ... */
  | (
      Delete,
      ZOperand(
        CursorE(_, EmptyHole(_)) as zhole,
        (prefix, A(Space, suffix_tl)),
      ),
    )
      when ZExp.is_after_zoperand(zhole) =>
    let S(operand, new_suffix) = suffix_tl;
    let zoperand = operand |> ZExp.place_before_operand;
    let new_zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
    ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
    |> wrap_in_AnaDone;

  /* Construction */

  /* contruction of Float Operators from Int Operators */
  | (Construct(SChar(".")), ZOperator((pos, oper), seq)) =>
    let new_operator = {
      switch (oper) {
      | Operators_Exp.Plus => Some(Operators_Exp.FPlus)
      | Operators_Exp.Minus => Some(Operators_Exp.FMinus)
      | Operators_Exp.Times => Some(Operators_Exp.FTimes)
      | Operators_Exp.Divide => Some(Operators_Exp.FDivide)
      | Operators_Exp.LessThan => Some(Operators_Exp.FLessThan)
      | Operators_Exp.GreaterThan => Some(Operators_Exp.FGreaterThan)
      | Operators_Exp.Equals => Some(Operators_Exp.FEquals)
      | _ => None
      };
    };
    switch (new_operator) {
    | Some(op) =>
      let new_zoperator = (pos, op);
      let new_zseq = ZSeq.ZOperator(new_zoperator, seq);
      Succeeded(AnaDone(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty)));
    | None => Failed
    };

  /* construction on operators either becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator(zoperator, _))
      when ZExp.is_after_zoperator(zoperator) =>
    ana_perform_opseq(ctx, MoveRight, (zopseq, u_gen), ty)
  /* ...or construction after movement */
  | (Construct(_), ZOperator(zoperator, _)) =>
    let move_cursor =
      ZExp.is_before_zoperator(zoperator)
        ? ZExp.move_cursor_left_zopseq : ZExp.move_cursor_right_zopseq;
    switch (zopseq |> move_cursor) {
    | None => Failed
    | Some(zopseq) => ana_perform_opseq(ctx, a, (zopseq, u_gen), ty)
    };

  | (Construct(SOp(SComma)), _)
      when
        ZExp.is_after_zopseq(zopseq)
        && !(zopseq |> has_Comma)
        && List.length(HTyp.get_prod_elements(ty)) >= 2 =>
    let (opseq, u_gen) =
      Statics_Exp.ana_fix_holes_opseq(
        ctx,
        u_gen,
        zopseq |> ZExp.erase_zopseq,
        // safe because pattern guard
        ty |> HTyp.get_prod_elements |> List.hd,
      );
    let (ZOpSeq(_, new_zseq), u_gen) = complete_tuple(u_gen, opseq, ty);
    Succeeded(AnaDone(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty)));

  | (Construct(SLine), ZOperand(zoperand, (prefix, A(_) as suffix)))
      when zoperand |> ZExp.is_after_zoperand =>
    let (new_line, u_gen) = {
      let operand = zoperand |> ZExp.erase_zoperand;
      let seq = Seq.prefix_seq(prefix, S(operand, E));
      let (opseq, u_gen) = mk_and_ana_fix_OpSeq(ctx, u_gen, seq, ty);
      (UHExp.ExpLine(opseq), u_gen);
    };
    let (new_zline, u_gen) = {
      let (hole, u_gen) = u_gen |> UHExp.new_EmptyHole;
      let seq = Seq.seq_suffix(S(hole, E), suffix);
      let (opseq, u_gen) = mk_and_ana_fix_OpSeq(ctx, u_gen, seq, ty);
      (ZExp.ExpLineZ(opseq |> ZExp.place_before_opseq), u_gen);
    };
    let new_zblock = ([new_line], new_zline, []);
    Succeeded(AnaDone((new_zblock, u_gen)));

  | (
      Construct(SLine),
      ZOperand(CursorE(_, op) as zoperand, (A(_) as prefix, suffix)),
    ) =>
    let (new_line, u_gen) = {
      let (hole, u_gen) =
        switch (op) {
        | EmptyHole(_) => (op, u_gen)
        | _ => UHExp.new_EmptyHole(u_gen)
        };
      let seq = Seq.prefix_seq(prefix, S(hole, E));
      let (opseq, _, u_gen) = mk_and_syn_fix_OpSeq(ctx, u_gen, seq);
      (UHExp.ExpLine(opseq), u_gen);
    };
    let (new_zline, u_gen) = {
      let zseq = ZSeq.ZOperand(zoperand, (E, suffix));
      let (zopseq, u_gen) =
        // TODO fix hack
        switch (mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty)) {
        | (([], ExpLineZ(zopseq), []), u_gen) => (zopseq, u_gen)
        | _ => assert(false)
        };
      (ZExp.ExpLineZ(zopseq), u_gen);
    };
    let new_zblock = ([new_line], new_zline, []);
    Succeeded(AnaDone((new_zblock, u_gen)));

  | (
      Construct(SOp(SSpace)),
      ZOperand(
        CursorE(_, Var(_, InVarHole(Keyword(k), _), _)) as zoperand,
        surround,
      ),
    )
      when ZExp.is_after_zoperand(zoperand) =>
    let (zhole, u_gen) = ZExp.new_EmptyHole(u_gen);
    let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zhole, surround));
    ana_perform_opseq(ctx, keyword_action(k), (zopseq, u_gen), ty);

  | (Construct(SOp(os)), ZOperand(zoperand, surround))
      when
        ZExp.is_before_zoperand(zoperand) || ZExp.is_after_zoperand(zoperand) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(operator) =>
      let construct_operator =
        ZExp.is_before_zoperand(zoperand)
          ? construct_operator_before_zoperand
          : construct_operator_after_zoperand;
      let (zseq, u_gen) =
        construct_operator(u_gen, operator, zoperand, surround);
      Succeeded(AnaDone(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty)));
    }
  /* Swap actions */
  | (SwapUp | SwapDown, ZOperator(_))
  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorE(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorE(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
    |> wrap_in_AnaDone;
  | (SwapRight, ZOperand(CursorE(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorE(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, new_zseq, ty))
    |> wrap_in_AnaDone;

  /* Zipper */

  | (_, ZOperand(zoperand, (E, E))) =>
    ana_perform_operand(ctx, a, (zoperand, u_gen), ty)

  | (_, ZOperand(zoperand, (prefix, suffix) as surround)) =>
    let n = Seq.length_of_affix(prefix);
    switch (
      Statics_Exp.ana_nth_type_mode(ctx, n, zopseq |> ZExp.erase_zopseq, ty)
    ) {
    | None => Failed
    | Some(Syn) =>
      switch (Statics_Exp.syn_operand(ctx, ZExp.erase_zoperand(zoperand))) {
      | None => Failed
      | Some(ty_zoperand) =>
        switch (syn_perform_operand(ctx, a, (zoperand, ty_zoperand, u_gen))) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform_opseq(
            ctx,
            Action_common.escape(side),
            (zopseq, u_gen),
            ty,
          )
        | Succeeded(SynExpands(r)) =>
          let (prefix_lines, u_gen) = lines_of_prefix(r.u_gen, prefix);
          let (new_subject, u_gen) =
            switch (r.subject, suffix) {
            | (
                [ExpLine(OpSeq(_, S(EmptyHole(_), E)))],
                A(Space, S(subject, suffix)),
              ) =>
              // if expanding keyword action is invoked on
              // empty hole followed by space, then drop
              // the empty hole and space
              resurround(u_gen, UHExp.Block.wrap(subject), (E, suffix))
            | _ => resurround(u_gen, r.subject, (E, suffix))
            };
          Succeeded(
            AnaExpands({
              ...r,
              u_gen,
              prefix: prefix_lines,
              subject: new_subject,
              suffix: [],
            }),
          );
        | Succeeded(SynDone((ze, _, u_gen))) =>
          let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
          Succeeded(
            AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
          );
        }
      }
    | Some(Ana(ty_zoperand)) =>
      switch (ana_perform_operand(ctx, a, (zoperand, u_gen), ty_zoperand)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_opseq(
          ctx,
          Action_common.escape(side),
          (zopseq, u_gen),
          ty,
        )
      | Succeeded(AnaExpands(r)) =>
        let (prefix_lines, u_gen) = lines_of_prefix(r.u_gen, prefix);
        let (new_subject, u_gen) =
          resurround(u_gen, r.subject, (E, suffix));
        Succeeded(
          AnaExpands({
            ...r,
            u_gen,
            prefix: prefix_lines,
            subject: new_subject,
            suffix: [],
          }),
        );
      | Succeeded(AnaDone((ze, u_gen))) =>
        let (new_ze, u_gen) = resurround_z(u_gen, ze, surround);
        Succeeded(
          AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)),
        );
      }
    };
  | (Init, _) => failwith("Init action should not be performed.")
  }
and ana_perform_operand =
    (
      ctx: Contexts.t,
      a: Action.t,
      (zoperand, u_gen): (ZExp.zoperand, MetaVarGen.t),
      ty: HTyp.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (a, zoperand) {
  /* Invalid cursor positions */
  | (
      _,
      CursorE(
        OnDelim(_) | OnOp(_),
        Var(_) | InvalidText(_, _) | IntLit(_) | FloatLit(_) | BoolLit(_) |
        ApPalette(_),
      ) |
      CursorE(
        OnText(_) | OnOp(_),
        EmptyHole(_) | ListNil(_) | Lam(_) | Inj(_) | Case(_) |
        Parenthesized(_) |
        ApPalette(_),
      ),
    ) =>
    Failed

  | (_, CursorE(cursor, operand))
      when !ZExp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  | _ when ZExp.is_inconsistent(zoperand) =>
    let zoperand' = zoperand |> ZExp.set_err_status_zoperand(NotInHole);
    let err = ZExp.get_err_status_zoperand(zoperand);
    let operand' = zoperand' |> ZExp.erase_zoperand;
    switch (Statics_Exp.syn_operand(ctx, operand')) {
    | None => Failed
    | Some(ty') =>
      switch (syn_perform_operand(ctx, a, (zoperand', ty', u_gen))) {
      | (Failed | CursorEscaped(_)) as outcome => outcome
      | Succeeded(SynExpands(r)) => Succeeded(AnaExpands(r))
      | Succeeded(SynDone((ze', ty', u_gen))) =>
        if (HTyp.consistent(ty', ty)) {
          // prune unnecessary type annotation
          let ze' =
            switch (ze') {
            | (
                [] as prefix,
                ExpLineZ(
                  ZOpSeq(skel, ZOperand(zoperand, (E, E) as surround)),
                ),
                [] as suffix,
              ) => (
                prefix,
                ZExp.ExpLineZ(ZOpSeq(skel, ZOperand(zoperand, surround))),
                suffix,
              )
            | _ => ze'
            };
          Succeeded(AnaDone((ze', u_gen)));
        } else if (HTyp.get_prod_arity(ty') != HTyp.get_prod_arity(ty)
                   && HTyp.get_prod_arity(ty) > 1) {
          let (err, u_gen) =
            ErrStatus.make_recycled_InHole(err, WrongLength, u_gen);
          let new_ze = ze' |> ZExp.set_err_status(err);
          Succeeded(AnaDone((new_ze, u_gen)));
        } else {
          let (err, u_gen) =
            ErrStatus.make_recycled_InHole(err, TypeInconsistent, u_gen);
          let new_ze = ze' |> ZExp.set_err_status(err);
          Succeeded(AnaDone((new_ze, u_gen)));
        }
      }
    };

  /* Movement */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    ana_move(ctx, a, (ZExp.ZBlock.wrap(zoperand), u_gen), ty)

  /* Invalid actions at the expression level */
  | (Construct(SList), CursorE(_)) => Failed

  | (Construct(SCloseSquareBracket), CursorE(_, _)) => Failed

  /* Backspace & Delete */

  | (Backspace, CursorE(_, EmptyHole(_) as operand)) =>
    let ze = operand |> ZExp.place_before_operand |> ZExp.ZBlock.wrap;
    ze |> ZExp.is_after
      ? Succeeded(AnaDone((ze, u_gen))) : CursorEscaped(Before);
  | (Delete, CursorE(_, EmptyHole(_) as operand)) =>
    let ze = operand |> ZExp.place_after_operand |> ZExp.ZBlock.wrap;
    ze |> ZExp.is_before
      ? Succeeded(AnaDone((ze, u_gen))) : CursorEscaped(After);

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorE(OnDelim(_, Before), _)) =>
    ana_perform_operand(ctx, MoveLeft, (zoperand, u_gen), ty)
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorE(OnDelim(_, After), _)) =>
    ana_perform_operand(ctx, MoveRight, (zoperand, u_gen), ty)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorE(OnDelim(k, Before), operand)) =>
    let new_ze = ZExp.ZBlock.wrap(CursorE(OnDelim(k, After), operand));
    ana_perform(ctx, Backspace, (new_ze, u_gen), ty) |> wrap_in_AnaDone;

  | (Backspace, CursorE(OnDelim(_, After), ListNil(_))) =>
    let (zhole, u_gen) = u_gen |> ZExp.new_EmptyHole;
    Succeeded(AnaDone((ZExp.ZBlock.wrap(zhole), u_gen)));

  | (Delete, CursorE(OnText(j), InvalidText(_, t))) =>
    ana_delete_text(ctx, u_gen, j, t, ty)
  | (Delete, CursorE(OnText(j), Var(_, _, x))) =>
    ana_delete_text(ctx, u_gen, j, x, ty)
  | (Delete, CursorE(OnText(j), IntLit(_, n))) =>
    ana_delete_text(ctx, u_gen, j, n, ty)
  | (Delete, CursorE(OnText(j), FloatLit(_, f))) =>
    ana_delete_text(ctx, u_gen, j, f, ty)
  | (Delete, CursorE(OnText(j), BoolLit(_, b))) =>
    ana_delete_text(ctx, u_gen, j, string_of_bool(b), ty)

  | (Backspace, CursorE(OnText(j), InvalidText(_, t))) =>
    ana_backspace_text(ctx, u_gen, j, t, ty)
  | (Backspace, CursorE(OnText(j), Var(_, _, x))) =>
    ana_backspace_text(ctx, u_gen, j, x, ty)
  | (Backspace, CursorE(OnText(j), IntLit(_, n))) =>
    ana_backspace_text(ctx, u_gen, j, n, ty)
  | (Backspace, CursorE(OnText(j), FloatLit(_, f))) =>
    ana_backspace_text(ctx, u_gen, j, f, ty)
  | (Backspace, CursorE(OnText(j), BoolLit(_, b))) =>
    ana_backspace_text(ctx, u_gen, j, string_of_bool(b), ty)

  /* \x :<| Int . x + 1   ==>   \x| . x + 1 */
  | (Backspace, CursorE(OnDelim(1, After), Lam(_, p, body))) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let (p, body_ctx, u_gen) =
        Statics_Pat.ana_fix_holes(ctx, u_gen, p, ty1);
      let (body, u_gen) =
        Statics_Exp.ana_fix_holes(body_ctx, u_gen, body, ty2);
      let new_ze =
        ZExp.ZBlock.wrap(LamZP(NotInHole, ZPat.place_after(p), body));
      Succeeded(AnaDone((new_ze, u_gen)));
    }
  | (
      Backspace,
      CursorE(
        OnDelim(k, After),
        (Lam(_, _, e) | Inj(_, _, e) | Case(_, e, _) | Parenthesized(e)) as operand,
      ),
    ) =>
    let place_cursor =
      switch (operand) {
      | Lam(_) =>
        switch (k) {
        | 0
        | 1 => ZExp.place_before
        | _two => ZExp.place_after
        }
      | _ =>
        switch (k) {
        | 0 => ZExp.place_before
        | _ => ZExp.place_after
        }
      };
    let new_ze = e |> place_cursor;
    Succeeded(AnaDone(Statics_Exp.ana_fix_holes_z(ctx, u_gen, new_ze, ty)));

  /* TODO consider deletion of type ascription on case */

  /* Construction */

  | (Construct(SCommentLine), _) =>
    if (ZExp.is_before_zoperand(zoperand)) {
      let operand = ZExp.erase_zoperand(zoperand);
      Succeeded(
        AnaDone((
          (
            [],
            CursorL(OnText(0), CommentLine("")),
            [ExpLine(OpSeq.wrap(operand))],
          ),
          u_gen,
        )),
      );
    } else {
      Failed;
    }

  | (Construct(SChar(s)), CursorE(_, EmptyHole(_))) =>
    ana_insert_text(ctx, u_gen, (0, s), "", ty)
  | (Construct(SChar(s)), CursorE(OnText(j), InvalidText(_, t))) =>
    ana_insert_text(ctx, u_gen, (j, s), t, ty)
  | (Construct(SChar(s)), CursorE(OnText(j), Var(_, _, x))) =>
    ana_insert_text(ctx, u_gen, (j, s), x, ty)
  | (Construct(SChar(s)), CursorE(OnText(j), IntLit(_, n))) =>
    ana_insert_text(ctx, u_gen, (j, s), n, ty)
  | (Construct(SChar(s)), CursorE(OnText(j), FloatLit(_, f))) =>
    ana_insert_text(ctx, u_gen, (j, s), f, ty)
  | (Construct(SChar(s)), CursorE(OnText(j), BoolLit(_, b))) =>
    ana_insert_text(ctx, u_gen, (j, s), string_of_bool(b), ty)
  | (Construct(SChar(_)), CursorE(_)) => Failed

  | (Construct(SCase), CursorE(_, operand)) =>
    Succeeded(
      mk_AnaExpandsToCase(~u_gen, ~scrut=UHExp.Block.wrap(operand), ()),
    )
  | (Construct(SLet), CursorE(_, operand)) =>
    Succeeded(
      mk_AnaExpandsToLet(~u_gen, ~def=UHExp.Block.wrap(operand), ()),
    )

  // TODO consider relaxing guards and
  // merging with regular op construction
  | (Construct(SOp(sop)), CursorE(OnText(j), InvalidText(_, t)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, sop, t, ty)
  | (Construct(SOp(sop)), CursorE(OnText(j), Var(_, _, x)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, sop, x, ty)
  | (Construct(SOp(sop)), CursorE(OnText(j), BoolLit(_, b)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, sop, string_of_bool(b), ty)
  | (Construct(SOp(sop)), CursorE(OnText(j), IntLit(_, n)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, sop, n, ty)
  | (Construct(SOp(sop)), CursorE(OnText(j), FloatLit(_, f)))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    ana_split_text(ctx, u_gen, j, sop, f, ty)

  | (Construct(SAnn), CursorE(_)) => Failed

  | (Construct(SParenthesized), CursorE(_, EmptyHole(_) as hole))
      when List.length(HTyp.get_prod_elements(ty)) >= 2 =>
    let (zopseq, u_gen) = complete_tuple(u_gen, OpSeq.wrap(hole), ty);
    let new_ze =
      ZExp.ParenthesizedZ(ZExp.ZBlock.wrap'(zopseq)) |> ZExp.ZBlock.wrap;
    Succeeded(AnaDone((new_ze, u_gen)));
  | (Construct(SParenthesized), CursorE(_)) =>
    let new_ze =
      ZExp.ZBlock.wrap(ParenthesizedZ(ZExp.ZBlock.wrap(zoperand)));
    Succeeded(AnaDone((new_ze, u_gen)));

  | (Construct(SInj(side)), CursorE(_)) =>
    switch (HTyp.matched_sum(ty)) {
    | Some((tyL, tyR)) =>
      let ty1 = InjSide.pick(side, tyL, tyR);
      let (zbody, u_gen) =
        Statics_Exp.ana_fix_holes_z(
          ctx,
          u_gen,
          ZExp.ZBlock.wrap(zoperand),
          ty1,
        );
      let new_ze = ZExp.ZBlock.wrap(InjZ(NotInHole, side, zbody));
      Succeeded(AnaDone((new_ze, u_gen)));
    | None =>
      let (zbody, _, u_gen) =
        Statics_Exp.syn_fix_holes_z(ctx, u_gen, ZExp.ZBlock.wrap(zoperand));
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let new_ze =
        ZExp.ZBlock.wrap(InjZ(InHole(TypeInconsistent, u), side, zbody));
      Succeeded(AnaDone((new_ze, u_gen)));
    }

  | (Construct(SLam), CursorE(_)) =>
    let body = ZExp.(ZExp.ZBlock.wrap(zoperand) |> erase);
    switch (HTyp.matched_arrow(ty)) {
    | Some((_, ty2)) =>
      let (body, u_gen) = Statics_Exp.ana_fix_holes(ctx, u_gen, body, ty2);
      let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
      let new_ze =
        ZExp.ZBlock.wrap(LamZP(NotInHole, ZOpSeq.wrap(zhole), body));
      Succeeded(AnaDone((new_ze, u_gen)));
    | None =>
      let (body, _, u_gen) = Statics_Exp.syn_fix_holes(ctx, u_gen, body);
      let (zhole, u_gen) = u_gen |> ZPat.new_EmptyHole;
      let (u, u_gen) = u_gen |> MetaVarGen.next;
      let new_ze =
        ZExp.ZBlock.wrap(
          LamZP(InHole(TypeInconsistent, u), ZOpSeq.wrap(zhole), body),
        );
      Succeeded(AnaDone((new_ze, u_gen)));
    };

  | (Construct(SCloseBraces), LamZE(_, _, zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      AnaDone((
        ZExp.ZBlock.wrap(
          ZExp.place_after_operand(ZExp.erase_zoperand(zoperand)),
        ),
        u_gen,
      )),
    )
  | (
      Construct(SCloseBraces),
      CursorE(OnDelim(2, Before), Lam(_, _, _) as lam),
    ) =>
    Succeeded(
      AnaDone((
        ZExp.ZBlock.wrap(ZExp.CursorE(OnDelim(2, After), lam)),
        u_gen,
      )),
    )
  | (Construct(SCloseBraces), CursorE(_, _)) => Failed

  | (Construct(SCloseParens), InjZ(err, side, zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      AnaDone((
        ZExp.ZBlock.wrap(
          ZExp.CursorE(
            OnDelim(1, After),
            Inj(err, side, ZExp.erase(zblock)),
          ),
        ),
        u_gen,
      )),
    )

  | (Construct(SCloseParens), ParenthesizedZ(zblock))
      when ZExp.is_after_zblock(zblock) =>
    Succeeded(
      AnaDone((
        ZExp.ZBlock.wrap(
          ZExp.CursorE(
            OnDelim(1, After),
            Parenthesized(ZExp.erase_zblock(zblock)),
          ),
        ),
        u_gen,
      )),
    )

  | (
      Construct(SCloseParens),
      CursorE(
        OnDelim(1, Before),
        Parenthesized(_) as operand | Inj(_, _, _) as operand,
      ),
    ) =>
    Succeeded(
      AnaDone((
        ZExp.ZBlock.wrap(ZExp.CursorE(OnDelim(1, After), operand)),
        u_gen,
      )),
    )
  | (Construct(SCloseParens), CursorE(_, _)) => Failed

  | (Construct(SOp(SSpace)), CursorE(OnDelim(_, After), _))
      when !ZExp.is_after_zoperand(zoperand) =>
    ana_perform_operand(ctx, MoveRight, (zoperand, u_gen), ty)

  | (Construct(SOp(os)), CursorE(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(operator) =>
      let construct_operator =
        ZExp.is_before_zoperand(zoperand)
          ? construct_operator_before_zoperand
          : construct_operator_after_zoperand;
      let (zseq, u_gen) =
        construct_operator(u_gen, operator, zoperand, (E, E));
      ActionOutcome.Succeeded(mk_and_ana_fix_ZOpSeq(ctx, u_gen, zseq, ty))
      |> wrap_in_AnaDone;
    }

  // TODO: consider how this interacts with subsumption case below
  | (Construct(_), CursorE(OnDelim(_, side), _))
      when
        !ZExp.is_before_zoperand(zoperand)
        && !ZExp.is_after_zoperand(zoperand) =>
    switch (
      ana_perform(
        ctx,
        Action_common.escape(side),
        (ZExp.ZBlock.wrap(zoperand), u_gen),
        ty,
      )
    ) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded(new_edit_state) =>
      ana_perform(ctx, a, new_edit_state, ty) |> wrap_in_AnaDone
    }

  | (Construct(SLine), CursorE(_)) when ZExp.is_before_zoperand(zoperand) =>
    let new_ze = (
      [UHExp.EmptyLine],
      ZExp.ExpLineZ(ZOpSeq.wrap(zoperand)),
      [],
    );
    Succeeded(AnaDone((new_ze, u_gen)));
  | (Construct(SLine), CursorE(_, op))
      when ZExp.is_after_zoperand(zoperand) =>
    let (new_hole, u_gen) =
      switch (op) {
      | EmptyHole(_) => (op, u_gen)
      | _ => UHExp.new_EmptyHole(u_gen)
      };
    let new_zline =
      UHExp.ExpLine(OpSeq.wrap(new_hole)) |> ZExp.place_before_line;
    let new_ze = (
      [
        UHExp.ExpLine(zoperand |> ZExp.erase_zoperand |> OpSeq.wrap)
        |> UHExp.Line.prune_empty_hole,
      ],
      new_zline,
      [],
    );
    Succeeded(AnaDone((new_ze, u_gen)));
  | (Construct(SLine), CursorE(_)) => Failed

  /* Invalid Swap actions */
  | (SwapUp | SwapDown, CursorE(_) | LamZP(_)) => Failed
  | (SwapLeft | SwapRight, CursorE(_)) => Failed

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (ana_perform(ctx, a, (zbody, u_gen), ty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      ana_perform_operand(
        ctx,
        Action_common.escape(side),
        (zoperand, u_gen),
        ty,
      )
    | Succeeded((zbody, u_gen)) =>
      let new_ze = ZExp.ZBlock.wrap(ParenthesizedZ(zbody));
      Succeeded(AnaDone((new_ze, u_gen)));
    }
  | (_, LamZP(_, zp, body)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      switch (Action_Pat.ana_perform(ctx, u_gen, a, zp, ty1_given)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, u_gen),
          ty,
        )
      | Succeeded((zp, ctx, u_gen)) =>
        let (body, u_gen) = Statics_Exp.ana_fix_holes(ctx, u_gen, body, ty2);
        let new_ze = ZExp.ZBlock.wrap(LamZP(NotInHole, zp, body));
        Succeeded(AnaDone((new_ze, u_gen)));
      }
    }
  | (_, LamZE(_, p, zbody)) =>
    switch (HTyp.matched_arrow(ty)) {
    | None => Failed
    | Some((ty1_given, ty2)) =>
      switch (Statics_Pat.ana(ctx, p, ty1_given)) {
      | None => Failed
      | Some(ctx_body) =>
        switch (ana_perform(ctx_body, a, (zbody, u_gen), ty2)) {
        | Failed => Failed
        | CursorEscaped(side) =>
          ana_perform_operand(
            ctx,
            Action_common.escape(side),
            (zoperand, u_gen),
            ty,
          )
        | Succeeded((zbody, u_gen)) =>
          let new_ze = ZExp.ZBlock.wrap(LamZE(NotInHole, p, zbody));
          Succeeded(AnaDone((new_ze, u_gen)));
        }
      }
    }
  | (_, InjZ(_, side, zbody)) =>
    switch (HTyp.matched_sum(ty)) {
    | None => Failed
    | Some((ty1, ty2)) =>
      let picked = InjSide.pick(side, ty1, ty2);
      switch (ana_perform(ctx, a, (zbody, u_gen), picked)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, u_gen),
          ty,
        )
      | Succeeded((zbody, u_gen)) =>
        let new_ze = ZExp.ZBlock.wrap(InjZ(NotInHole, side, zbody));
        Succeeded(AnaDone((new_ze, u_gen)));
      };
    }
  | (_, CaseZE(_, zscrut, rules)) =>
    switch (Statics_Exp.syn(ctx, zscrut |> ZExp.erase)) {
    | None => Failed
    | Some(ty1) =>
      switch (syn_perform(ctx, a, (zscrut, ty1, u_gen))) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, u_gen),
          ty,
        )
      | Succeeded((zscrut, ty1, u_gen)) =>
        let (rules, u_gen) =
          Statics_Exp.ana_fix_holes_rules(ctx, u_gen, rules, ty1, ty);
        let new_ze =
          ZExp.ZBlock.wrap(
            CaseZE(StandardErrStatus(NotInHole), zscrut, rules),
          );
        Succeeded(AnaDone((new_ze, u_gen)));
      }
    }
  | (_, CaseZR(_, scrut, zrules)) =>
    switch (Statics_Exp.syn(ctx, scrut)) {
    | None => Failed
    | Some(pat_ty) =>
      switch (ana_perform_rules(ctx, a, (zrules, u_gen), pat_ty, ty)) {
      | Failed => Failed
      | CursorEscaped(side) =>
        ana_perform_operand(
          ctx,
          Action_common.escape(side),
          (zoperand, u_gen),
          ty,
        )
      | Succeeded((new_zrules, u_gen)) =>
        let new_ze =
          ZExp.ZBlock.wrap(
            CaseZR(StandardErrStatus(NotInHole), scrut, new_zrules),
          );
        Succeeded(AnaDone((new_ze, u_gen)));
      }
    }

  /* Subsumption */
  | (UpdateApPalette(_) | Construct(SApPalette(_) | SListNil), _)
  | (_, ApPaletteZ(_)) => ana_perform_subsume(ctx, a, (zoperand, u_gen), ty)
  /* Invalid actions at the expression level */
  | (Init, _) => failwith("Init action should not be performed.")
  }
and ana_perform_subsume =
    (
      ctx: Contexts.t,
      a: Action.t,
      (zoperand: ZExp.zoperand, u_gen: MetaVarGen.t),
      ty: HTyp.t,
    )
    : ActionOutcome.t(ana_success) =>
  switch (Statics_Exp.syn_operand(ctx, ZExp.erase_zoperand(zoperand))) {
  | None => Failed
  | Some(ty1) =>
    // must call syn_perform_operand and not syn_perform
    // to pass along any keyword expansions
    switch (syn_perform_operand(ctx, a, (zoperand, ty1, u_gen))) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded(SynExpands(r)) => Succeeded(AnaExpands(r))
    | Succeeded(SynDone((ze, ty1, u_gen))) =>
      if (HTyp.consistent(ty, ty1)) {
        Succeeded(AnaDone((ze, u_gen)));
      } else {
        let (ze, u_gen) = ze |> ZExp.mk_inconsistent(u_gen);
        Succeeded(AnaDone((ze, u_gen)));
      }
    }
  };
