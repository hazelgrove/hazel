let operator_of_shape = (os: Action.operator_shape): option(UHTyp.operator) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SPlus
  | SVBar
  | SMinus
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
  };

let sumbody_operator_of_shape =
    (os: Action.operator_shape): option(UHTyp.sumbody_operator) =>
  switch (os) {
  | SPlus => Some(Plus)
  | SArrow
  | SComma
  | SVBar
  | SMinus
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

let shape_of_sumbody_operator =
    (op: UHTyp.sumbody_operator): Action.operator_shape =>
  switch (op) {
  | Plus => SPlus
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

let construct_zsumbody_operator =
    (
      u_gen: MetaVarGen.t,
      operator: UHTyp.sumbody_operator,
      zoperand: ZTyp.zsumbody_operand,
      (prefix, suffix): ZTyp.sumbody_operand_surround,
    )
    : (ZTyp.zsumbody, MetaVarGen.t) => {
  let (tag, u_gen) = UHTag.new_TagHole(u_gen);
  let ztag = ZTyp.ConstTagZ(ZTag.place_before(tag));
  let operand = zoperand |> ZTyp.erase_zsumbody_operand;
  let surround =
    if (ZTyp.is_before_zsumbody_operand(zoperand)) {
      (prefix, Seq.A(operator, S(operand, suffix)));
    } else {
      (Seq.A(operator, S(operand, prefix)), suffix);
    };
  (ZTyp.mk_sumbody_ZOpSeq(ZOperand(ztag, surround)), u_gen);
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
  }

and move_zsumbody =
    (u_gen: MetaVarGen.t, a: Action.t, zsumbody: ZTyp.zsumbody)
    : ActionOutcome.t((ZTyp.zsumbody, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (
      CursorPath_Typ.follow_sumbody(path, zsumbody |> ZTyp.erase_zsumbody)
    ) {
    | None => Failed
    | Some(zsumbody) => Succeeded((zsumbody, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.prev_hole_steps(
        CursorPath_Typ.holes_zsumbody(zsumbody, []),
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sumbody(
          steps,
          zsumbody |> ZTyp.erase_zsumbody,
        )
      ) {
      | None => Failed
      | Some(path) => move_zsumbody(u_gen, MoveTo(path), zsumbody)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(
        next_hole_steps(CursorPath_Typ.holes_zsumbody(zsumbody, []))
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sumbody(
          steps,
          zsumbody |> ZTyp.erase_zsumbody,
        )
      ) {
      | None => Failed
      | Some(path) => move_zsumbody(u_gen, MoveTo(path), zsumbody)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left_zsumbody(zsumbody)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, u_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right_zsumbody(zsumbody)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, u_gen))
    }
  | UpdateApPalette(_)
  | Delete
  | Backspace
  | Construct(_)
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
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_),
      ) |
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

  | (Construct(SOp(SPlus)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      Succeeded((ZTyp.mk_ZOpSeq(ZSeq.insert(zseq, surround)), u_gen))
    }

  /* Space becomes a tag hole in an empty sum body */
  | (
      Construct(SOp(SSpace)),
      ZOperand(
        CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)),
        surround,
      ),
    ) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTag.place_cursor(OnDelim(0, After), tag_hole)) {
    | None => Failed
    | Some(ztag) =>
      let zsum = ZTyp.SumZ(ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)));
      let zty = ZTyp.mk_ZOpSeq(ZSeq.ZOperand(zsum, surround));
      Succeeded((zty, u_gen));
    };

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded((construct_operator(op, zoperand, surround), u_gen))
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
  | (_, ZOperand(zoperand, surround)) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      Succeeded((ZTyp.mk_ZOpSeq(ZSeq.insert(zseq, surround)), u_gen))
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_operand =
    (u_gen: MetaVarGen.t, a: Action.t, zoperand: ZTyp.zoperand)
    : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_) |
        SCommentLine,
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

  /* Empty sums */

  // sum {<| }  ==>  |?
  | (Backspace, CursorT(OnDelim(0, After), Sum(None))) =>
    let ty_hole = OpSeq.wrap(UHTyp.Hole);
    Succeeded((ZTyp.place_before(ty_hole), u_gen));

  // sum { <|}  ==>  sum {<| }
  | (Backspace, CursorT(OnDelim(1, Before), Sum(None) as operand)) =>
    perform_operand(u_gen, a, CursorT(OnDelim(0, After), operand))

  // sum { }<|  ==>  sum { |}
  | (Backspace, CursorT(OnDelim(1, After), Sum(None) as operand)) =>
    switch (ZTyp.place_cursor_operand(OnDelim(1, Before), operand)) {
    | Some(zty) => Succeeded((ZOpSeq.wrap(zty), u_gen))
    | None => Failed
    }

  // |>sum { }  ==>  |?
  | (Delete, CursorT(OnDelim(0, Before), Sum(None))) =>
    let ty_hole = OpSeq.wrap(UHTyp.Hole);
    Succeeded((ZTyp.place_before(ty_hole), u_gen));

  // sum {|> }  ==>  sum { |>}
  | (Delete, CursorT(OnDelim(0, After), Sum(None) as operand)) =>
    perform_operand(u_gen, a, CursorT(OnDelim(1, Before), operand))

  // sum { |>}  ==>  ?|
  | (Delete, CursorT(OnDelim(1, Before), Sum(None))) =>
    let ty_hole = OpSeq.wrap(UHTyp.Hole);
    Succeeded((ZTyp.place_after(ty_hole), u_gen));

  | (
      Construct(SChar(c)),
      CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)),
    ) =>
    UHTag.is_majuscule_letter(c.[0])
      ? {
        let ztag = ZTag.place_after(UHTag.Tag(c));
        let zty =
          ZOpSeq.wrap(ZTyp.SumZ(ZOpSeq.wrap(ZTyp.ConstTagZ(ztag))));
        Succeeded((zty, u_gen));
      }
      : Failed

  // sum {| }  ==>  sum { ? + ?| }
  // sum { |}  ==>  sum { ? + ?| }
  | (
      Construct(SOp(SPlus)),
      CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)),
    ) =>
    let (tag_hole1, u_gen) = UHTag.new_TagHole(u_gen);
    let (tag_hole2, u_gen) = UHTag.new_TagHole(u_gen);
    let tag1 = UHTyp.ConstTag(tag_hole1);
    let ztag2 = ZTyp.ConstTagZ(ZTag.place_after(tag_hole2));
    let zseq =
      ZSeq.ZOperand(ztag2, (Seq.A(Operators_SumBody.Plus, S(tag1, E)), E));
    let zsumbody = ZTyp.mk_sumbody_ZOpSeq(zseq);
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));

  /* All other actions on empty sums are invalid */
  | (_, CursorT(_, Sum(None))) => Failed

  /* Non-empty sums */

  // sum {<| _ }  ==>  ?|
  | (Backspace, CursorT(OnDelim(0, After), Sum(Some(_)))) =>
    let ty_hole = OpSeq.wrap(UHTyp.Hole);
    Succeeded((ZTyp.place_after(ty_hole), u_gen));

  // sum { _ <|}  ==>  sum { _| }
  | (Backspace, CursorT(OnDelim(1, Before), Sum(Some(sumbody)))) =>
    let zsumbody = ZTyp.place_after_sumbody(sumbody);
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));

  // sum { _ }<|  ==>  sum { _ |}
  | (Backspace, CursorT(OnDelim(_1, After), Sum(Some(_)) as operand)) =>
    switch (ZTyp.place_cursor_operand(OnDelim(1, Before), operand)) {
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
    | None => Failed
    }

  // |>sum { _ }  ==>  |?
  | (Delete, CursorT(OnDelim(0, Before), Sum(Some(_)))) =>
    let ty_hole = OpSeq.wrap(UHTyp.Hole);
    Succeeded((ZTyp.place_before(ty_hole), u_gen));

  // sum {|> _ }  ==>  sum { |_ }
  | (Delete, CursorT(OnDelim(0, After), Sum(Some(sumbody)))) =>
    let zsumbody = ZTyp.place_before_sumbody(sumbody);
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));

  // sum { _ |>}  ==>  sum { _ }|
  | (Delete, CursorT(OnDelim(1, Before), Sum(Some(_)) as operand)) =>
    switch (ZTyp.place_cursor_operand(OnDelim(1, After), operand)) {
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
    | None => Failed
    }

  // |?  ==>  sum { |? }
  // ?|  ==>  sum { ?| }
  | (Construct(SOp(SPlus)), CursorT(OnDelim(_, side), Hole)) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTag.place_cursor(OnDelim(0, side), tag_hole)) {
    | Some(ztag) =>
      let zsumbody = ZOpSeq.wrap(ZTyp.ConstTagZ(ztag));
      let zty = ZOpSeq.wrap(ZTyp.SumZ(zsumbody));
      Succeeded((zty, u_gen));
    | None => Failed
    };

  // sum {| _ }  ==>  sum { |? + _ }
  | (
      Construct(SOp(SPlus)),
      CursorT(OnDelim(0, After), Sum(Some(OpSeq(_, seq)))),
    ) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let ztag = ZTyp.ConstTagZ(ZTag.place_before(tag_hole));
    let zseq = ZSeq.ZOperand(ztag, (E, A(Operators_SumBody.Plus, seq)));
    let zsumbody = ZTyp.mk_sumbody_ZOpSeq(zseq);
    let zty = ZOpSeq.wrap(ZTyp.SumZ(zsumbody));
    Succeeded((zty, u_gen));

  // sum { _ |}  ==>  sum { _ + ?| }
  | (
      Construct(SOp(SPlus)),
      CursorT(OnDelim(1, Before), Sum(Some(OpSeq(_, seq)))),
    ) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let ztag = ZTyp.ConstTagZ(ZTag.place_after(tag_hole));
    let zseq =
      ZSeq.ZOperand(ztag, (A(Operators_SumBody.Plus, Seq.rev(seq)), E));
    let zsumbody = ZTyp.mk_sumbody_ZOpSeq(zseq);
    let zty = ZOpSeq.wrap(ZTyp.SumZ(zsumbody));
    Succeeded((zty, u_gen));

  /* |ty  ==>  sum { ?(| ty ) } */
  /* ty|  ==>  sum { ?( ty| ) } */
  | (Construct(SOp(SPlus)), CursorT(_, operand)) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    let zoperand = ZTyp.place_before_operand(operand);
    let zsumbody = ZOpSeq.wrap(ZTyp.ArgTagZA(tag, ZOpSeq.wrap(zoperand)));
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));

  /* All other actions on non-empty are invalid */
  | (_, CursorT(_, Sum(Some(_)))) => Failed

  /* Zippered sums */

  | (_, SumZ(zsumbody)) =>
    switch (perform_zsumbody(u_gen, a, zsumbody)) {
    | Failed => Failed
    | Succeeded((zsumbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen))
    | CursorEscaped(side) as result =>
      switch (a) {
      | Delete
      | Backspace =>
        // sum { <|? }  ==> sum {| }
        // sum { ?|> }  ==> sum { |}
        let destructive_escape = (): ActionOutcome.t((ZTyp.t, MetaVarGen.t)) => {
          let cursor: CursorPosition.t =
            switch (side) {
            | Side.Before => OnDelim(0, After)
            | Side.After => OnDelim(1, Before)
            };
          let sum = UHTyp.Sum(None);
          switch (ZTyp.place_cursor_operand(cursor, sum)) {
          | Some(zsum) => Succeeded((ZOpSeq.wrap(zsum), u_gen))
          | None => Failed
          };
        };
        switch (a, side) {
        | (Backspace, Before)
        | (Delete, After) => destructive_escape()
        | (_, _) =>
          // non-destructive escape
          perform_operand(u_gen, Action_common.escape(side), zoperand)
        };
      | _ => result
      }
    }

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

  | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), u_gen))

  | (Backspace, CursorT(OnDelim(_, After), Unit | Int | Float | Bool)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), u_gen))

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

  | (Construct(SChar("I")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Int)), u_gen))
  | (Construct(SChar("F")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Float)), u_gen))
  | (Construct(SChar("B")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Bool)), u_gen))
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))), u_gen))

  | (Construct(SParenthesized), CursorT(_)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
      u_gen,
    ))

  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded((construct_operator(op, zoperand, (E, E)), u_gen))
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
  }

and perform_zsumbody =
    (
      u_gen: MetaVarGen.t,
      a: Action.t,
      ZOpSeq(skel, zseq) as zsumbody: ZTyp.zsumbody,
    )
    : ActionOutcome.t((ZTyp.zsumbody, MetaVarGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions at the top level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement is handled separately */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumbody(u_gen, a, zsumbody)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_zsumbody(u_gen, Action_common.escape(side), zsumbody)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_zsumbody(
      u_gen,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    )

  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, prefix_tl) = prefix;
    let S(suffix_hd, suffix_tl) = suffix;
    switch (suffix_hd) {
    // ... + _1 +<| ? + _2 + ...   ==>   ... + _1| + _2 + ...
    | ConstTag(TagHole(_)) =>
      let zoperand = ZTyp.place_after_sumbody_operand(prefix_hd);
      let zsumbody' = ZSeq.ZOperand(zoperand, (prefix_tl, suffix_tl));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zsumbody'), u_gen));
    // _1 +<| _2 + ...   ==>   |_2 + ...
    // ... + _1 +<| _2 + ...   ==>   ... +| _2 + ...
    | ConstTag(Tag(_))
    | ArgTag(_, _) =>
      let new_prefix =
        switch (prefix_tl) {
        | E => Seq.S(prefix_hd, E)
        | A(_, new_prefix) => new_prefix
        };
      switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
      | None => Failed
      | Some(zoperator) =>
        let zsumbody' = ZSeq.ZOperator(zoperator, (new_prefix, suffix));
        Succeeded((ZTyp.mk_sumbody_ZOpSeq(zsumbody'), u_gen));
      };
    };

  /* Construction */

  // ... + |_ ( _ ) + ...  ==>  ... + |? + _ ( _ ) + ...
  | (
      Construct(SOp(SPlus)),
      ZOperand(ArgTagZT(ztag, ty), (prefix, suffix)),
    )
      when ZTag.is_before(ztag) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let lhs_zoperand = ZTyp.ConstTagZ(ZTag.place_before(tag_hole));
    let rhs_operand = UHTyp.ArgTag(ZTag.erase(ztag), ty);
    let new_suffix =
      Seq.A(Operators_SumBody.Plus, Seq.S(rhs_operand, suffix));
    let zseq = ZSeq.ZOperand(lhs_zoperand, (prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  // ... + _| ( _ ) + ...  ==>  ... + _ + ?| ( _ ) + ...
  | (
      Construct(SOp(SPlus)),
      ZOperand(ArgTagZT(ztag, ty), (prefix, suffix)),
    )
      when ZTag.is_after(ztag) =>
    let lhs_operand = UHTyp.ConstTag(ZTag.erase(ztag));
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let rhs_zoperand = ZTyp.ArgTagZT(ZTag.place_after(tag_hole), ty);
    let new_prefix =
      Seq.A(Operators_SumBody.Plus, Seq.S(lhs_operand, prefix));
    let zseq = ZSeq.ZOperand(rhs_zoperand, (new_prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  // ... + _ ( _|_ ) + ...  ==>  ... + _ ( _ ) + ?| + ...
  | (
      Construct(SOp(SPlus)),
      ZOperand(ArgTagZA(_, zty) as zoperand, (prefix, suffix)),
    )
      when !ZTyp.is_cursor_in_sum(zty) =>
    let new_prefix =
      Seq.A(
        Operators_SumBody.Plus,
        Seq.S(ZTyp.erase_zsumbody_operand(zoperand), prefix),
      );
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let new_zoperand = ZTyp.ConstTagZ(ZTag.place_before(tag_hole));
    let zseq = ZSeq.ZOperand(new_zoperand, (new_prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  // ... + |_ + ...  ==>  ... + |_ + _ + ...
  // ... + _| + ...  ==>  ... + _ + _| + ...
  | (Construct(SOp(SPlus)), ZOperand(zoperand, (prefix, suffix))) =>
    let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
    let place_cursor =
      ZTyp.is_before_zsumbody_operand(zoperand)
        ? ZTag.place_before : ZTag.place_after;

    let new_zoperand = ZTyp.ConstTagZ(place_cursor(tag_hole));
    let operand = ZTyp.erase_zsumbody_operand(zoperand);
    let (new_prefix, new_suffix) =
      ZTyp.is_before_zsumbody_operand(zoperand)
        ? (prefix, Seq.A(Operators_SumBody.Plus, Seq.S(operand, suffix)))
        : (Seq.A(Operators_SumBody.Plus, Seq.S(operand, prefix)), suffix);
    let zseq = ZSeq.ZOperand(new_zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  // _ +| _  ==>  _ + |_
  | (Construct(SOp(SSpace)), ZOperator((OnOp(_), _), _)) =>
    perform_zsumbody(u_gen, MoveRight, zsumbody)

  | (Construct(_), ZOperator(_, _)) => Failed

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_))
  | (SwapLeft, ZOperand(CursorArgTag(_, _, _), (E, _)))
  | (SwapRight, ZOperand(CursorArgTag(_), (_, E))) => Failed

  | (
      SwapLeft,
      ZOperand(
        (CursorArgTag(_) | ConstTagZ(_) | ArgTagZT(_, _)) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));

  | (
      SwapLeft,
      ZOperand(
        ArgTagZA(_, zty) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    )
      when ZTyp.is_before(zty) || ZTyp.is_after(zty) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));

  | (
      SwapRight,
      ZOperand(
        (CursorArgTag(_) | ConstTagZ(_) | ArgTagZT(_, _)) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));

  | (
      SwapRight,
      ZOperand(
        ArgTagZA(_, zty) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    )
      when ZTyp.is_before(zty) || ZTyp.is_after(zty) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_zsumbody_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody(u_gen, Action_common.escape(side), zsumbody)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_sumbody_ZOpSeq(
            ZOperand(zoperand, (new_prefix, new_suffix)),
          ),
          u_gen,
        ));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_sumbody_ZOpSeq(
            ZOperator(zoperator, (new_prefix, new_suffix)),
          ),
          u_gen,
        ));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_zsumbody_operand =
    (u_gen: MetaVarGen.t, a: Action.t, zoperand: ZTyp.zsumbody_operand)
    : ActionOutcome.t((ZTyp.zsumbody, MetaVarGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SOp(SPlus) |
        SApPalette(_) |
        SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorArgTag(OnText(_) | OnOp(_), _, _)) => Failed
  | (_, CursorArgTag(cursor, tag, ty))
      when !ZTyp.is_valid_cursor_sumbody_operand(cursor, ArgTag(tag, ty)) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumbody(u_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* _ <|( _ )   ==>   _| ( _ ) */
  /* _ ( _ <|)   ==>   _ ( _| ) */
  /* _ <|   ==>   _| */
  | (Backspace, CursorArgTag(OnDelim(_, Before), _, _)) =>
    zoperand |> ZTyp.is_before_zsumbody_operand
      ? CursorEscaped(Before)
      : perform_zsumbody_operand(u_gen, MoveLeft, zoperand)

  /* |> _ ( _ )   ==>   |_ ( _ ) */
  /* _ (|> _ )   ==>   _ ( |_ ) */
  /* |> _   ==>   |_ */
  | (Delete, CursorArgTag(OnDelim(_, After), _, _)) =>
    zoperand |> ZTyp.is_after_zsumbody_operand
      ? CursorEscaped(After)
      : perform_zsumbody_operand(u_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorArgTag(OnDelim(k, Before), tag, ty)) =>
    perform_zsumbody_operand(
      u_gen,
      Backspace,
      CursorArgTag(OnDelim(k, After), tag, ty),
    )

  /* _ ( _ )<|   ==>   _ | */
  /* _ (<| _ )   ==>   _ | */
  | (Backspace, CursorArgTag(OnDelim(_, After), tag, _)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.place_after_sumbody_operand(ConstTag(tag))),
      u_gen,
    ))

  /* Construction */

  | (Construct(SOp(SSpace)), CursorArgTag(OnDelim(_, _), _, _)) =>
    perform_zsumbody_operand(u_gen, MoveRight, zoperand)

  | (
      Construct(SOp(_) | SChar(_) | SList | SParenthesized),
      CursorArgTag(_, _, _),
    ) =>
    Failed

  // _|  ==>  _ ( |? )
  | (Construct(SParenthesized), ConstTagZ(ztag)) =>
    let tag = ZTag.erase(ztag);
    let zty = ZTyp.place_before(UHTyp.mk_OpSeq(Seq.wrap(UHTyp.Hole)));
    let zoperand = ZTyp.ArgTagZA(tag, zty);
    Succeeded((ZOpSeq.wrap(zoperand), u_gen));

  | (Construct(SOp(SSpace)), ConstTagZ(ztag)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)), u_gen))
    }

  | (Construct(SChar(_)), ConstTagZ(ztag)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)), u_gen))
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorArgTag(_, _, _)) => Failed

  /* Zipper Cases */
  | (_, ConstTagZ(ztag)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)), u_gen))
    }
  | (_, ArgTagZT(ztag, ty)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZT(ztag, ty)), u_gen))
    }
  | (_, ArgTagZA(tag, zty)) =>
    switch (perform(u_gen, a, zty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zty, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZA(tag, zty)), u_gen))
    }

  | (Init, _) => failwith("Init action should not be performed.")
  };
