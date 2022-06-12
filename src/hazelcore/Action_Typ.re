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

let construct_operator =
    (
      u_gen: MetaVarGen.t,
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
    )
    : (ZTyp.zopseq, MetaVarGen.t) => {
  let operand = zoperand |> ZTyp.erase_zoperand;
  let (zoperand, surround, u_gen) =
    if (ZTyp.is_before_zoperand(zoperand)) {
      let (hole, u_gen) = UHTyp.new_EmptyHole(u_gen);
      let zoperand = hole |> ZTyp.place_before_operand; // TODO anand raef: use UHTyp.new_EmptyHole(u_gen) here... Need to add u_gen as input to construct_operator() and other funcs in this file
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix), u_gen);
    } else {
      let (hole, u_gen) = UHTyp.new_EmptyHole(u_gen);
      let zoperand = hole |> ZTyp.place_before_operand; // TODO anand raef: placeholder id
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix), u_gen);
    };
  (ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround)), u_gen);
};

let rec move =
        (u_gen: MetaVarGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded((zty, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), zty)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), zty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, u_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, u_gen))
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

let rec perform =
        (u_gen: MetaVarGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  perform_opseq(u_gen, a, zty)
and perform_opseq =
    (
      u_gen: MetaVarGen.t,
      a: Action.t,
      ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq,
    )
    : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
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
    move(u_gen, a, zopseq)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_opseq(u_gen, Action_common.escape(side), zopseq)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_opseq(
      u_gen,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Succeeded((
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      u_gen,
    ));

  /* Construction */
  /* construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(u_gen, MoveRight, zopseq)
  /* ...or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(u_gen, Action_common.escape(side), zopseq)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, u_gen)) => perform(u_gen, a, zty)
    }

  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(u_gen, MoveRight, zopseq)

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      let (zopseq, u_gen) =
        construct_operator(u_gen, op, zoperand, surround);
      Succeeded((zopseq, u_gen));
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
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), u_gen));
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
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), u_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
          u_gen,
        ));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
          u_gen,
        ));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and perform_operand =
    (u_gen: MetaVarGen.t, a: Action.t, zoperand: ZTyp.zoperand)
    : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      Construct(
        SAnn | SLet | SLine | SFun | SListNil | SInj(_) | SCase | SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before) : perform_operand(u_gen, MoveLeft, zoperand)
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After) : perform_operand(u_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(k, Before), operand)) =>
    perform_operand(u_gen, Backspace, CursorT(OnDelim(k, After), operand))

  | (Backspace, CursorT(OnDelim(_, After), Hole(_) as hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(hole)), u_gen))

  | (Backspace, CursorT(OnDelim(_, After), Unit | Int | Float | Bool)) =>
    let (hole, u_gen) = UHTyp.new_EmptyHole(u_gen);
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(hole)), u_gen)); // TODO anand raef: placeholder id, use new_EmptyHole

  /* ( _ )<|  ==>  _| */
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    Succeeded((body |> place_cursor, u_gen));

  /* Construction */

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_operand(u_gen, MoveRight, zoperand)
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (perform_operand(u_gen, Action_common.escape(side), zoperand)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, u_gen)) => perform(u_gen, a, zty)
    }

  | (Construct(SChar("I")), CursorT(_, Hole(_))) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Int)), u_gen))
  | (Construct(SChar("F")), CursorT(_, Hole(_))) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Float)), u_gen))
  | (Construct(SChar("B")), CursorT(_, Hole(_))) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Bool)), u_gen))
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))), u_gen))

  | (Construct(SCloseSquareBracket), ListZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), UHTyp.List(ZTyp.erase(zopseq))),
      ),
      u_gen,
    ))
  | (Construct(SCloseSquareBracket), CursorT(_, _)) => Failed

  | (Construct(SParenthesized), CursorT(_)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
      u_gen,
    ))

  | (Construct(SCloseBraces), CursorT(_)) => Failed

  | (Construct(SCloseParens), ParenthesizedZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), Parenthesized(ZTyp.erase(zopseq))),
      ),
      u_gen,
    ))
  | (
      Construct(SCloseParens),
      CursorT(OnDelim(1, Before), Parenthesized(opseq)),
    ) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(OnDelim(1, After), Parenthesized(opseq))),
      u_gen,
    ))
  | (Construct(SCloseParens), CursorT(_, _)) => Failed
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      let (zopseq, u_gen) = construct_operator(u_gen, op, zoperand, (E, E));
      Succeeded((zopseq, u_gen));
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (perform(u_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)), u_gen))
    }
  | (_, ListZ(zbody)) =>
    switch (perform(u_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ListZ(zbody)), u_gen))
    }
  | (Init, _) => failwith("Init action should not be performed.")
  };
