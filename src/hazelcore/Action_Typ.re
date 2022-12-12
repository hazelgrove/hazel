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
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
    )
    : ZTyp.zopseq => {
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
  ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround));
};

let rec move = (a: Action.t, zty: ZTyp.t): ActionOutcome.t(ZTyp.t) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded(zty)
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
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
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(MoveTo(path), zty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded(z)
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded(z)
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

let rec perform = (a: Action.t, zty: ZTyp.t): ActionOutcome.t(ZTyp.t) =>
  perform_opseq(a, zty)
and perform_opseq =
    (a: Action.t, ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq)
    : ActionOutcome.t(ZTyp.t) =>
  switch (a, zseq) {
  /* Invalid actions at the type level */
  | (
      Construct(SAnn | SLet | SLine | SFun | SListNil | SInj(_) | SCase) |
      SwapUp |
      SwapDown,
      _,
    ) /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) =>
    Failed /* Movement handled at top level */

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(a, zopseq) /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_opseq(Action_common.escape(side), zopseq) /* Delete before operator == Backspace after operator */

  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_opseq(
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    ) /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */

  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Succeeded(ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix)))) /* construction on operators becomes movement... */ /* Construction */;

  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(MoveRight, zopseq) /* ...or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(Action_common.escape(side), zopseq)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded(zty) => perform(a, zty)
    } /* Space becomes movement until we have proper type constructors */

  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(MoveRight, zopseq)

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) => Succeeded(construct_operator(op, zoperand, surround))
    } /* SwapLeft and SwapRight is handled at block level */

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
    Succeeded(ZTyp.mk_ZOpSeq(new_zseq));
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
    Succeeded(ZTyp.mk_ZOpSeq(new_zseq)) /* Zipper */;

  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_operand(a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(Action_common.escape(side), zopseq)
    | Succeeded(ZOpSeq(_, zseq)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded(
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
        );
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded(
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
        );
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and perform_operand =
    (a: Action.t, zoperand: ZTyp.zoperand): ActionOutcome.t(ZTyp.t) =>
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
    Failed /* Invalid cursor positions */

  | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed /* Movement handled at top level */

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(a, ZOpSeq.wrap(zoperand)) /* ( _ <|)   ==>   ( _| ) */ /* Backspace and Delete */

  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before)
      : perform_operand(MoveLeft, zoperand) /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After)
      : perform_operand(MoveRight, zoperand) /* Delete before delimiter == Backspace after delimiter */

  | (Delete, CursorT(OnDelim(k, Before), operand)) =>
    perform_operand(Backspace, CursorT(OnDelim(k, After), operand))

  | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.place_before_operand(Hole)))

  | (Backspace, CursorT(OnDelim(_, After), Unit | Int | Float | Bool)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.place_before_operand(Hole))) /* (<| _ )  ==>  |_ */ /* ( _ )<|  ==>  _| */

  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    Succeeded(body |> place_cursor) /* Construction */;

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_operand(MoveRight, zoperand)
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (perform_operand(Action_common.escape(side), zoperand)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded(zty) => perform(a, zty)
    }

  | (Construct(SChar("I")), CursorT(_, Hole)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.place_after_operand(Int)))
  | (Construct(SChar("F")), CursorT(_, Hole)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.place_after_operand(Float)))
  | (Construct(SChar("B")), CursorT(_, Hole)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.place_after_operand(Bool)))
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))))

  | (Construct(SCloseSquareBracket), ListZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded(
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), UHTyp.List(ZTyp.erase(zopseq))),
      ),
    )
  | (Construct(SCloseSquareBracket), CursorT(_, _)) => Failed

  | (Construct(SParenthesized), CursorT(_)) =>
    Succeeded(ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))))

  | (Construct(SCloseBraces), CursorT(_)) => Failed

  | (Construct(SCloseParens), ParenthesizedZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded(
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), Parenthesized(ZTyp.erase(zopseq))),
      ),
    )
  | (
      Construct(SCloseParens),
      CursorT(OnDelim(1, Before), Parenthesized(opseq)),
    ) =>
    Succeeded(
      ZOpSeq.wrap(ZTyp.CursorT(OnDelim(1, After), Parenthesized(opseq))),
    )
  | (Construct(SCloseParens), CursorT(_, _)) => Failed
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) => Succeeded(construct_operator(op, zoperand, (E, E)))
    } /* Invalid SwapLeft and SwapRight actions */

  | (SwapLeft | SwapRight, CursorT(_)) => Failed /* Zipper Cases */

  | (_, ParenthesizedZ(zbody)) =>
    switch (perform(a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(Action_common.escape(side), zoperand)
    | Succeeded(zbody) =>
      Succeeded(ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)))
    }
  | (_, ListZ(zbody)) =>
    switch (perform(a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(Action_common.escape(side), zoperand)
    | Succeeded(zbody) => Succeeded(ZOpSeq.wrap(ZTyp.ListZ(zbody)))
    }
  | (Init, _) => failwith("Init action should not be performed.")
  };
