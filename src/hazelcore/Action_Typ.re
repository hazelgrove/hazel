let operator_of_shape = (os: Action.operator_shape): option(UHTyp.operator) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SVBar => Some(Sum)
  | SMinus
  | SPlus
  | STimes
  | SDivide
  | SAnd
  | SOr
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SCons => None
  };

let shape_of_operator = (op: UHTyp.operator): Action.operator_shape =>
  switch (op) {
  | Arrow => SArrow
  | Prod => SComma
  | Sum => SVBar
  };

let text_operand =
    (ctx: Context.t, shape: TyTextShape.t, id_gen: IDGen.t)
    : (UHTyp.operand, Context.t, IDGen.t) =>
  switch (shape) {
  | Int => (Int, ctx, id_gen)
  | Bool => (Bool, ctx, id_gen)
  | Float => (Float, ctx, id_gen)
  | ExpandingKeyword(kw) =>
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let t = ExpandingKeyword.to_string(kw);
    (TyVar(InHole(Reserved, u), t), ctx, id_gen);
  | TyVar(t) =>
    switch (Context.tyvar_ref(ctx, t)) {
    | None =>
      let (u, id_gen) = IDGen.next_hole(id_gen);
      (TyVar(InHole(Unbound, u), t), ctx, id_gen);
    | Some(cref) =>
      let k = Kind.singleton(HTyp.tyvar(ctx, cref.index, t));
      let ctx = Context.add_tyvar(ctx, t, k);
      (TyVar(NotInTyVarHole, t), ctx, id_gen);
    }
  };

let construct_operator =
    (
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
      id_gen: IDGen.t,
    )
    : (ZTyp.zopseq, IDGen.t) => {
  let operand = zoperand |> ZTyp.erase_zoperand;
  let (zoperand, surround) =
    if (ZTyp.is_before_zoperand(zoperand)) {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix));
    } else {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix));
    };
  (ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround)), id_gen);
};

let mk_syn_text =
    (ctx: Context.t, id_gen: IDGen.t, caret_index: int, text: string)
    : ActionOutcome.t((ZTyp.t, IDGen.t)) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TyTextShape.of_string(text)) {
  | None =>
    if (StringUtil.is_empty(text)) {
      Succeeded((
        ZOpSeq.wrap(ZTyp.CursorT(OnDelim(0, Before), Hole)),
        id_gen,
      ));
    } else {
      let (u, id_gen) = IDGen.next_hole(id_gen);
      let ty = UHTyp.TyVar(InHole(InvalidText, u), text);
      let zty = ZOpSeq.wrap(ZTyp.CursorT(text_cursor, ty));
      Succeeded((zty, id_gen));
      /* Failed; */
    }
  | Some(Bool) =>
    let zty = ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Bool));
    Succeeded((zty, id_gen));
  | Some(Int) =>
    let zty = ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Int));
    Succeeded((zty, id_gen));
  | Some(Float) =>
    let zty = ZOpSeq.wrap(ZTyp.CursorT(text_cursor, Float));
    Succeeded((zty, id_gen));
  | Some(ExpandingKeyword(kw)) =>
    let (u, id_gen) = id_gen |> IDGen.next_hole;
    let zty =
      ZOpSeq.wrap(
        ZTyp.CursorT(
          text_cursor,
          UHTyp.TyVar(InHole(Reserved, u), ExpandingKeyword.to_string(kw)),
        ),
      );
    Succeeded((zty, id_gen));
  | Some(TyVar(t)) =>
    let (err: TyVarErrStatus.t, id_gen) =
      switch (Context.tyvar_ref(ctx, t)) {
      | None =>
        let (u, id_gen) = IDGen.next_hole(id_gen);
        (InHole(Unbound, u), id_gen);
      | Some(_) => (NotInTyVarHole, id_gen)
      };
    let zty = ZOpSeq.wrap(ZTyp.CursorT(text_cursor, TyVar(err, t)));
    Succeeded((zty, id_gen));
  };
};

let rec move = (a: Action.t, zty: ZTyp.t): ActionOutcome.t(ZTyp.t) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, ZTyp.erase(zty))) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.prev_hole_steps(CursorPath_Typ.holes_z(zty, []))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, ZTyp.erase(zty))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), zty)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, ZTyp.erase(zty))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), zty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => CursorEscaped(Before)
    | Some(zty) => Succeeded(zty)
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => CursorEscaped(After)
    | Some(zty) => Succeeded(zty)
    }
  | Construct(_)
  | Delete
  | Backspace
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

let insert_text = Action_common.syn_insert_text_(~mk_syn_text);
let backspace_text = Action_common.syn_backspace_text_(~mk_syn_text);
let delete_text = Action_common.syn_delete_text_(~mk_syn_text);

let split_text =
    (
      ctx: Context.t,
      id_gen: IDGen.t,
      caret_index: int,
      sop: Action.operator_shape,
      text: string,
    )
    : option((ZTyp.t, Context.t, IDGen.t)) => {
  let (l, r) = StringUtil.split_string(caret_index, text);
  switch (
    TyTextShape.of_string(l),
    operator_of_shape(sop),
    TyTextShape.of_string(r),
  ) {
  | (None, _, _)
  | (_, None, _)
  | (_, _, None) => None
  | (Some(lshape), Some(op), Some(rshape)) =>
    let (loperand, ctx, id_gen) = text_operand(ctx, lshape, id_gen);
    let (roperand, ctx, id_gen) = text_operand(ctx, rshape, id_gen);
    let zoperand = ZTyp.place_before_operand(roperand);
    let zty =
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (A(op, S(loperand, E)), E)));
    Some((zty, ctx, id_gen));
  };
};

let rec perform =
        (ctx: Context.t, a: Action.t, zty: ZTyp.t, id_gen: IDGen.t)
        : ActionOutcome.t((ZTyp.t, Context.t, IDGen.t)) =>
  perform_opseq(ctx, a, zty, id_gen)

and perform_opseq =
    (
      ctx: Context.t,
      a: Action.t,
      ZOpSeq(skel, zseq) as zopseq,
      id_gen: IDGen.t,
    )
    : ActionOutcome.t((ZTyp.t, Context.t, IDGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions at the type level */
  | (
      Construct(SAnn | SLet | SLine | SFun | SListNil | SInj(_) | SCase) |
      SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    switch (move(a, zopseq)) {
    | (Failed | CursorEscaped(_)) as outcome => outcome
    | Succeeded(zty) => Succeeded((zty, ctx, id_gen))
    }

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_opseq(ctx, Action_common.escape(side), zopseq, id_gen)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_opseq(
      ctx,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
      id_gen,
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Succeeded((
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      ctx,
      id_gen,
    ));

  /* Construction */
  /* construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(ctx, MoveRight, zopseq, id_gen)
  /* ...or construction after movement */
  | (Construct(_), ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(ctx, Action_common.escape(side), zopseq, id_gen)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, ctx, id_gen)) => perform(ctx, a, zty, id_gen)
    }

  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(ctx, MoveRight, zopseq, id_gen)

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      let (zty, id_gen) = construct_operator(op, zoperand, surround, id_gen);
      Succeeded((zty, ctx, id_gen));
    }

  /* SwapLeft and SwapRight is handled at block level */

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), ctx, id_gen));
  | (SwapRight, ZOperand(CursorT(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), ctx, id_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_operand(ctx, a, zoperand, id_gen)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(ctx, Action_common.escape(side), zopseq, id_gen)
    | Succeeded((ZOpSeq(_, zseq), ctx, id_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
          ctx,
          id_gen,
        ));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
          ctx,
          id_gen,
        ));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_operand =
    (ctx: Context.t, a: Action.t, zoperand: ZTyp.zoperand, id_gen: IDGen.t)
    : ActionOutcome.t((ZTyp.t, Context.t, IDGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      Construct(
        SAnn | SLet | STyAlias | SLine | SFun | SListNil | SInj(_) | SCase |
        SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    switch (move(a, ZOpSeq.wrap(zoperand))) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(ctx, Action_common.escape(side), zoperand, id_gen)
    | Succeeded(zty) => Succeeded((zty, ctx, id_gen))
    }

  /* Backspace and Delete */

  | (Backspace, CursorT(OnText(caret_index), Int)) =>
    switch (backspace_text(ctx, id_gen, caret_index, "Int")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Backspace, CursorT(OnText(caret_index), Bool)) =>
    switch (backspace_text(ctx, id_gen, caret_index, "Bool")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Backspace, CursorT(OnText(caret_index), Float)) =>
    switch (backspace_text(ctx, id_gen, caret_index, "Float")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }

  | (Delete, CursorT(OnText(caret_index), Int)) =>
    switch (delete_text(ctx, id_gen, caret_index, "Int")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Delete, CursorT(OnText(caret_index), Bool)) =>
    switch (delete_text(ctx, id_gen, caret_index, "Bool")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Delete, CursorT(OnText(caret_index), Float)) =>
    switch (delete_text(ctx, id_gen, caret_index, "Float")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }

  | (Delete, CursorT(OnDelim(_, Before), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Hole)), ctx, id_gen))
  | (Delete, CursorT(OnDelim(_, Before), Unit)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Hole)), ctx, id_gen))

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before)
      : perform_operand(ctx, MoveLeft, zoperand, id_gen)
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After)
      : perform_operand(ctx, MoveRight, zoperand, id_gen)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(k, Before), operand)) =>
    perform_operand(
      ctx,
      Backspace,
      CursorT(OnDelim(k, After), operand),
      id_gen,
    )

  | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), ctx, id_gen))
  | (Backspace, CursorT(OnDelim(_, After), Unit)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), ctx, id_gen))

  | (Backspace, CursorT(OnDelim(_, After), Int | Float | Bool | TyVar(_))) =>
    failwith("Impossible: Int|Float|Bool|TyVar are treated as text")

  /* TyVar-related Backspace & Delete */
  | (Delete, CursorT(OnText(caret_index), TyVar(_, t))) =>
    switch (delete_text(ctx, id_gen, caret_index, t)) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Backspace, CursorT(OnText(caret_index), TyVar(_, t))) =>
    switch (backspace_text(ctx, id_gen, caret_index, t)) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }

  /* ( _ )<|  ==>  _| */
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    Succeeded((place_cursor(body), ctx, id_gen));

  /* Construction */

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_operand(ctx, MoveRight, zoperand, id_gen)
  | (Construct(_), CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (
      perform_operand(ctx, Action_common.escape(side), zoperand, id_gen)
    ) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, ctx, id_gen)) => perform(ctx, a, zty, id_gen)
    }

  | (Construct(SChar(s)), CursorT(_, Hole)) =>
    switch (insert_text(ctx, id_gen, (0, s), "")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Construct(SChar(s)), CursorT(OnText(j), Int)) =>
    switch (insert_text(ctx, id_gen, (j, s), "Int")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Construct(SChar(s)), CursorT(OnText(j), Bool)) =>
    switch (insert_text(ctx, id_gen, (j, s), "Bool")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Construct(SChar(s)), CursorT(OnText(j), Float)) =>
    switch (insert_text(ctx, id_gen, (j, s), "Float")) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }

  | (Construct(SChar(s)), CursorT(OnText(j), TyVar(_, t))) =>
    switch (insert_text(ctx, id_gen, (j, s), t)) {
    | (Failed | CursorEscaped(_)) as result => result
    | Succeeded((zty, id_gen)) => Succeeded((zty, ctx, id_gen))
    }
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))),
      ctx,
      id_gen,
    ))

  | (Construct(SCloseSquareBracket), ListZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    let zty =
      ZTyp.CursorT(OnDelim(1, After), UHTyp.List(ZTyp.erase(zopseq)));
    Succeeded((ZOpSeq.wrap(zty), ctx, id_gen));
  | (Construct(SCloseSquareBracket), CursorT(_, _)) => Failed

  | (Construct(SParenthesized), CursorT(_)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
      ctx,
      id_gen,
    ))

  /* split */
  | (Construct(SOp(os)), CursorT(OnText(j), Int))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    split_text(ctx, id_gen, j, os, "Int") |> ActionOutcome.of_option
  | (Construct(SOp(os)), CursorT(OnText(j), Bool))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    split_text(ctx, id_gen, j, os, "Bool") |> ActionOutcome.of_option
  | (Construct(SOp(os)), CursorT(OnText(j), Float))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    split_text(ctx, id_gen, j, os, "Float") |> ActionOutcome.of_option

  | (Construct(SOp(os)), CursorT(OnText(j), TyVar(_, t)))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    split_text(ctx, id_gen, j, os, t) |> ActionOutcome.of_option

  | (Construct(SCloseBraces), CursorT(_)) => Failed

  | (Construct(SCloseParens), ParenthesizedZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    let zopseq =
      ZTyp.CursorT(OnDelim(1, After), Parenthesized(ZTyp.erase(zopseq)));
    Succeeded((ZOpSeq.wrap(zopseq), ctx, id_gen));
  | (
      Construct(SCloseParens),
      CursorT(OnDelim(1, Before), Parenthesized(opseq)),
    ) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(OnDelim(1, After), Parenthesized(opseq))),
      ctx,
      id_gen,
    ))
  | (Construct(SCloseParens), CursorT(_, _)) => Failed
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      let (zty, id_gen) = construct_operator(op, zoperand, (E, E), id_gen);
      Succeeded((zty, ctx, id_gen));
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (perform(ctx, a, zbody, id_gen)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(ctx, Action_common.escape(side), zoperand, id_gen)
    | Succeeded((zbody, ctx, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)), ctx, id_gen))
    }
  | (_, ListZ(zbody)) =>
    switch (perform(ctx, a, zbody, id_gen)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(ctx, Action_common.escape(side), zoperand, id_gen)
    | Succeeded((zbody, ctx, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ListZ(zbody)), ctx, id_gen))
    }

  /* Invalid cursor positions */
  | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  | (Init, _) => failwith("Init action should not be performed.")
  };
