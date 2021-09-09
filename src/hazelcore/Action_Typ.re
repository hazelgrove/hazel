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

let fix_holes = (zty: ZTyp.t, u_gen: MetaVarGen.t): (ZTyp.t, MetaVarGen.t) => {
  let path = CursorPath_Typ.of_z(zty);
  let (ty, u_gen) = UHTyp.fix_holes(ZTyp.erase(zty), u_gen);
  let zty =
    CursorPath_Typ.follow(path, ty)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zty, u_gen);
};

let mk_and_fix_OpSeq =
    (seq: UHTyp.seq, u_gen: MetaVarGen.t): (UHTyp.t, MetaVarGen.t) => {
  let opseq = UHTyp.mk_OpSeq(seq);
  UHTyp.fix_holes(opseq, u_gen);
};

let mk_and_fix_ZOpSeq =
    (zseq: ZTyp.zseq, u_gen: MetaVarGen.t): (ZTyp.t, MetaVarGen.t) => {
  let zopseq = ZTyp.mk_ZOpSeq(zseq);
  fix_holes(zopseq, u_gen);
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
  switch (perform_opseq(u_gen, a, zty)) {
  | (Failed | CursorEscaped(_)) as outcome => outcome
  | Succeeded((zty, u_gen)) => Succeeded(fix_holes(zty, u_gen))
  }

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
  // ..., [k-2], [k-1],<| [k], ...   ==>   ..., [k-2], [k-1]|, ...
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Succeeded((
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      u_gen,
    ));

  /* Construction */

  /*
   Pressing <Space> after an operator moves the cursor forward.

   ...,| _ ...  =( )=>  ..., |_ ...
   */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(u_gen, MoveRight, zopseq)

  /*
   When the cursor is on an operator, the cursor moves before construction.

   ...,| _ ...  =(,)=>  ..., |_ ...  =(,)=>  ..., ?,| _ ...
   ... _|, ...  =(,)=>  ... _|, ...  =(,)=>  ... _,| ?, ...
   */
  | (Construct(_), ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(u_gen, Action_common.escape(side), zopseq)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, u_gen)) => perform(u_gen, a, zty)
    }

  /*
   Pressing <Space> after an operand moves the cursor forward.

   ... _| ...  =( )=>  ... _ |...
   */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(u_gen, MoveRight, zopseq)

  /*
   Pressing <Space> inside an empty sum body constructs an empty tag hole.

   sum {| }  =( )=>  sum { |? }
   sum { |}  =( )=>  sum { ?| }
   */
  | (
      Construct(SOp(SSpace)),
      ZOperand(
        CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)) as zoperand,
        surround,
      ),
    ) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      Succeeded((ZTyp.mk_ZOpSeq(ZSeq.insert(zseq, surround)), u_gen))
    }

  | (Construct(SOp(SPlus)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      Succeeded((ZTyp.mk_ZOpSeq(ZSeq.insert(zseq, surround)), u_gen))
    }

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
  /* Invalid actions */
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

  /* Invalid actions on sums */
  | (
      Construct(
        SList | SParenthesized |
        SOp(
          SMinus | STimes | SDivide | SLessThan | SGreaterThan | SEquals |
          SComma |
          SArrow |
          SVBar |
          SCons |
          SAnd |
          SOr,
        ),
      ),
      CursorT(_, Sum(_)),
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

  /*
   Pressing <Space> in an empty sum body constructs a tag hole.

   sum {| }  =( )=>  sum { |? }
   sum { |}  =( )=>  sum { ?| }
   */
  | (
      Construct(SOp(SSpace)),
      CursorT(OnDelim(0 as j, After) | OnDelim(1 as j, Before), Sum(None)),
    ) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    let place_cursor = ZTag.(j == 0 ? place_before : place_after);
    let zsum = ZTyp.SumZ(ZOpSeq.wrap(ZTyp.ConstTagZ(place_cursor(tag))));
    Succeeded((ZOpSeq.wrap(zsum), u_gen));

  /*
   Pressing <Plus> inside a sum body delimiter wraps the sum in another sum.

   sum {| ... }  =(+)=>  sum { |?( sum { ... } ) }
   sum { ... |}  =(+)=>  sum { |?( sum { ... } ) }
   */
  | (
      Construct(SOp(SPlus)),
      CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(_) as operand),
    ) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTag.place_cursor(OnDelim(0, Before), tag)) {
    | None => Failed
    | Some(ztag) =>
      let zoperand = ZTyp.ArgTagZT(ztag, OpSeq.wrap(operand));
      Succeeded((ZOpSeq.wrap(ZTyp.SumZ(ZOpSeq.wrap(zoperand))), u_gen));
    };

  /*
   Pressing <Plus> outside a sum wraps the sum in another sum.

   |sum { ... }  =(+)=>  |sum { ?( sum { ... } ) }
   sum { ... }|  =(+)=>  sum { ?( sum { ... } ) }|
   */
  | (
      Construct(SOp(SPlus)),
      CursorT(
        (OnDelim(0, Before) | OnDelim(1, After)) as cursor,
        Sum(_) as operand,
      ),
    ) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    let sumbody = OpSeq.wrap(UHTyp.ArgTag(tag, OpSeq.wrap(operand)));
    switch (ZTyp.place_cursor_operand(cursor, UHTyp.Sum(Some(sumbody)))) {
    | None => Failed
    | Some(zty) => Succeeded((ZOpSeq.wrap(zty), u_gen))
    };

  /*
   Pressing a valid tag character inside an empty sum constructs a tag.

   sum { | }  =(c)=>  sum { c| }
   */
  | (
      Construct(SChar(c)),
      CursorT(OnDelim(0, After) | OnDelim(1, Before), Sum(None)),
    ) =>
    let (ztag, u_gen) = Action_Tag.mk_text(0, c, u_gen);
    let zty = ZOpSeq.wrap(ZTyp.SumZ(ZOpSeq.wrap(ZTyp.ConstTagZ(ztag))));
    Succeeded((zty, u_gen));

  /*
   Pressing <Plus> on a type hole constructs a tag hole inside a sum.

   |?  ==>  sum { |? }
   ?|  ==>  sum { ?| }
   */
  | (Construct(SOp(SPlus)), CursorT(OnDelim(_, side), Hole)) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTag.place_cursor(OnDelim(0, side), tag)) {
    | None => Failed
    | Some(ztag) =>
      let zsumbody = ZOpSeq.wrap(ZTyp.ConstTagZ(ztag));
      Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));
    };

  /*
   Pressing <Plus> on a type constructs an ArgTag, with the type as its
   argument, inside a sum.

   |_  ==>  sum { ?( |_ ) }
   _|  ==>  sum { ?( _| ) }
   */
  | (Construct(SOp(SPlus)), CursorT(_, _)) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    let zsumbody = ZOpSeq.wrap(ZTyp.ArgTagZA(tag, ZOpSeq.wrap(zoperand)));
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen));

  /*
   Destroying the inside of an empty sum body delimiter destroys the sum.

   sum { |>}  ==>  ?|
   sum {<| }  ==>  |?
   */
  | (Backspace, CursorT(OnDelim(0 as j, After), Sum(None)))
  | (Delete, CursorT(OnDelim(1 as j, Before), Sum(None))) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_operand : ZTyp.place_after_operand;
    Succeeded((ZOpSeq.wrap(place_cursor(UHTyp.Hole)), u_gen));

  /*
   Destroying the inside of a sum body delimiter destroys the sum body.
   */
  // sum { ... _ |>}  ==>  sum { |}
  // sum {<| _ ... }  ==>  sum {| }
  | (Backspace, CursorT(OnDelim(0, After) as cursor, Sum(Some(_))))
  | (Delete, CursorT(OnDelim(1, Before) as cursor, Sum(Some(_)))) =>
    switch (ZTyp.place_cursor_operand(cursor, Sum(None))) {
    | None => Failed
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
    }

  /*
   Destroying a sum body delimiter from the outside destroys the sum.

   |>sum { ... }  ==>  |?
   sum { ... }<|  ==>  ?|
   */
  | (Delete, CursorT(OnDelim(0 as j, Before), Sum(_)))
  | (Backspace, CursorT(OnDelim(j, After), Sum(_))) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_operand : ZTyp.place_after_operand;
    Succeeded((ZOpSeq.wrap(place_cursor(UHTyp.Hole)), u_gen));

  /*
   Destroying a singleton tag hole destroys the sum body.
   */
  // sum { |>? }  ==>  sum { |}
  // sum { ?<| }  ==>  sum {| }
  | (
      Delete,
      SumZ(
        ZOpSeq(
          _,
          ZOperand(
            ConstTagZ(
              CursorTag(OnDelim(_0, Before as side), EmptyTagHole(_)),
            ),
            (E, E),
          ),
        ),
      ),
    )
  | (
      Backspace,
      SumZ(
        ZOpSeq(
          _,
          ZOperand(
            ConstTagZ(
              CursorTag(OnDelim(_0, After as side), EmptyTagHole(_)),
            ),
            (E, E),
          ),
        ),
      ),
    ) =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_cursor_operand(OnDelim(1, Before))
      | After => ZTyp.place_cursor_operand(OnDelim(0, After))
      };
    switch (place_cursor(Sum(None))) {
    | None => Failed
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
    };

  /*
   When a destruction action is toward a sum body delimiter, the action becomes
   movement.
   */
  // sum {|> }  ==>  sum { |}
  // sum { <|}  ==>  sum {| }
  | (Delete, CursorT(OnDelim(0 as j, After), Sum(None) as operand))
  | (Backspace, CursorT(OnDelim(1 as j, Before), Sum(None) as operand)) =>
    let place_cursor =
      j == 0
        ? ZTyp.place_cursor_operand(OnDelim(1, Before))
        : ZTyp.place_cursor_operand(OnDelim(0, After));
    switch (place_cursor(operand)) {
    | None => Failed
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
    };

  /*
   When a desctructive action is toward a sum body, the action becomes movement.

   sum {|> _ ... }  ==>  sum { |_ ... }
   sum { ... _ <|}  ==>  sum { ... _| }
   */
  | (Delete, CursorT(OnDelim(0 as j, After), Sum(Some(sumbody))))
  | (Backspace, CursorT(OnDelim(1 as j, Before), Sum(Some(sumbody)))) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_sumbody : ZTyp.place_after_sumbody;
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(place_cursor(sumbody))), u_gen));

  /*
   When a destruction action is away from a sum, then action escapes.

   sum { ... }|>  ==>  CursorEscaped(After)
   <|sum { ... }  ==>  CursorEscaped(Before)
   */
  | (Delete, CursorT(OnDelim(1, After as side), Sum(_)))
  | (Backspace, CursorT(OnDelim(0, Before as side), Sum(_))) =>
    CursorEscaped(side)

  /* Zippered Sums */
  | (_, SumZ(zsumbody)) =>
    switch (perform_zsumbody(u_gen, a, zsumbody)) {
    | Failed as failed => failed
    | Succeeded((zsumbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumbody)), u_gen))
    | CursorEscaped(side) =>
      let place_cursor =
        switch (side) {
        | Before => ZTyp.place_cursor_operand(OnDelim(0, After))
        | After => ZTyp.place_cursor_operand(OnDelim(1, Before))
        };
      switch (place_cursor(UHTyp.Sum(Some(ZTyp.erase_zsumbody(zsumbody))))) {
      | None => Failed
      | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), u_gen))
      };
    }

  /* -- End of Sums -- */

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
      ZOpSeq(_, zseq) as zsumbody: ZTyp.zsumbody,
    )
    : ActionOutcome.t((ZTyp.zsumbody, MetaVarGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions on sum body operators */
  | (
      UpdateApPalette(_) |
      Construct(
        SCommentLine | SList | SAnn | SLam | SListNil | SInj | SLet | SLine |
        SCase |
        SApPalette(_) |
        SParenthesized |
        SChar(_) |
        SOp(
          SMinus | STimes | SDivide | SLessThan | SGreaterThan | SEquals |
          SComma |
          SArrow |
          SVBar |
          SCons |
          SAnd |
          SOr,
        ),
      ) |
      SwapUp |
      SwapDown,
      ZOperator(_, _),
    ) =>
    Failed

  /* Invalid swap actions */
  | (SwapLeft | SwapRight, ZOperator(_))
  | (SwapLeft, ZOperand(_, (E, _)))
  | (SwapRight, ZOperand(_, (_, E))) => Failed

  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement is handled separately */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumbody(u_gen, a, zsumbody)

  /*
   Pressing <Plus> between a tag and its argument splits them apart.

   ... _|( _ ) ...  =(+)=>  ... _ +| ?( _ ) ...
   */
  | (
      Construct(SOp(SPlus)),
      ZOperand(ArgTagZT(ztag, ty), (prefix, suffix)),
    )
      when ZTag.is_after(ztag) =>
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let (tag, u_gen) = UHTag.new_TagHole(u_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(ZTag.erase(ztag)), prefix);
      let new_suffix = Seq.S(UHTyp.ArgTag(tag, ty), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Plus> inside a ConstTag splits the tag.

   ... 1|2 ...  =(+)=>  ... 1 +| 2 ...
   */
  | (
      Construct(SOp(SPlus)),
      ZOperand(
        ConstTagZ(CursorTag(OnText(j), Tag(_, t)) as ztag),
        (prefix, suffix),
      ),
    )
      when !ZTag.(is_before(ztag) || is_after(ztag)) =>
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let n = String.length(t);
      let (tag1, u_gen) = Action_Tag.mk_tag(String.sub(t, 0, j), u_gen);
      let (tag2, u_gen) = Action_Tag.mk_tag(String.sub(t, j, n - j), u_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(tag1), prefix);
      let new_suffix = Seq.S(UHTyp.ConstTag(tag2), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Plus> inside an ArgTag tag splits the tag.

   ... 1|2( _ ) ...  =(+)=>  ... 1 +| 2( _ ) ...
   */
  | (
      Construct(SOp(SPlus)),
      ZOperand(
        ArgTagZT(CursorTag(OnText(j), Tag(_, t)) as ztag, ty),
        (prefix, suffix),
      ),
    )
      when !ZTag.(is_before(ztag) || is_after(ztag)) =>
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let n = String.length(t);
      let (tag1, u_gen) = Action_Tag.mk_tag(String.sub(t, 0, j), u_gen);
      let (tag2, u_gen) = Action_Tag.mk_tag(String.sub(t, j, n - j), u_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(tag1), prefix);
      let new_suffix = Seq.S(UHTyp.ArgTag(tag2, ty), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Plus> before a sum body operand constructs a tag hole.

   ... |_ ...  =(+)=>  ... ? +| _ ...
   */
  | (Construct(SOp(SPlus)), ZOperand(zoperand, (prefix, suffix)))
      when ZTyp.is_before_zsumbody_operand(zoperand) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let new_prefix = Seq.S(UHTyp.ConstTag(tag), prefix);
      let new_suffix = Seq.S(ZTyp.erase_zsumbody_operand(zoperand), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    };

  /*
   Pressing <Plus> after a sum body operand constructs a tag hole.

   ... _| ...  =(+)=>  ... _ +| ? ...
   */
  | (Construct(SOp(SPlus)), ZOperand(zoperand, (prefix, suffix)))
      when ZTyp.is_after_zsumbody_operand(zoperand) =>
    let (tag, u_gen) = UHTag.new_TagHole(u_gen);
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let new_prefix = Seq.S(ZTyp.erase_zsumbody_operand(zoperand), prefix);
      let new_suffix = Seq.S(UHTyp.ConstTag(tag), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    };

  /*
   Pressing <Plus> on a sum body operator constructs a tag hole.

   ... _ |+ _ ...  =(+)=>  ... _ +| ? + _ ...
   ... _ +| _ ...  =(+)=>  ... _ + ? +| _ ...
   */
  | (
      Construct(SOp(SPlus)),
      ZOperator((OnOp(side), op), (prefix, suffix)),
    ) =>
    switch (ZTyp.place_after_sumbody_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let (tag, u_gen) = UHTag.new_TagHole(u_gen);
      let surround =
        switch (side) {
        | Before => (prefix, Seq.S(UHTyp.ConstTag(tag), Seq.A(op, suffix)))
        | After => (Seq.S(UHTyp.ConstTag(tag), Seq.A(op, prefix)), suffix)
        };
      let zseq = ZSeq.ZOperator(zop, surround);
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Plus> in an ArgTag body applies the action to the body.

   ... + _ ( |_ ) + ...  =(+)=>  ... + _ ( sum { |_ } ) + ...
   ... + _ ( _| ) + ...  =(+)=>  ... + _ ( sum { _| } ) + ...
   */
  | (Construct(SOp(SPlus)), ZOperand(ArgTagZA(tag, zty), surround)) =>
    switch (perform(u_gen, a, zty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumbody(u_gen, Action_common.escape(side), zsumbody)
    | Succeeded((zty, u_gen)) =>
      let zseq = ZSeq.ZOperand(ZTyp.ArgTagZA(tag, zty), surround);
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Space> on either side of a sum body operator moves the cursor to
   the right.

   _ |+ _  =( )=>  _ +| _
   _ +| _  =( )=>  _ + |_
   */
  | (Construct(SOp(SSpace)), ZOperator(_, _)) =>
    perform_zsumbody(u_gen, MoveRight, zsumbody)

  /*
   Principled Destruction:
   1. Destroy holes before non-holes.
   2. Preserve the direction of action.
   */

  /*
   Pressing <Delete> after a sum body operator moves the cursor right.

   ... _ +|> _ ...  ==>  ... _ + |_ ...
   */
  | (Delete, ZOperator((OnOp(After), _), _)) =>
    move_zsumbody(u_gen, MoveRight, zsumbody)

  /*
   Pressing <Backspace> before a sum body operator moves the cursor left.

   ... _ <|+ _ ...  ==>  ... _| + _ ...
   */
  | (Backspace, ZOperator((OnOp(Before), _), _)) =>
    move_zsumbody(u_gen, MoveLeft, zsumbody)

  /*
   Pressing <Delete> after a sum body operand moves the cursor right.

   ... _|> + _ ...  ==>  ... _ |+ _ ...
   */
  | (Delete, ZOperand(zoperand, _))
      when ZTyp.is_after_zsumbody_operand(zoperand) =>
    move_zsumbody(u_gen, MoveRight, zsumbody)

  /*
   Pressing <Backspace> before a sum body operand moves the cursor left.

   ... _ + <|_ ...  ==>  ... _ +| _ ...
   */
  | (Backspace, ZOperand(zoperand, _))
      when ZTyp.is_before_zsumbody_operand(zoperand) =>
    move_zsumbody(u_gen, MoveLeft, zsumbody)

  /*
   Destroying a sum body operator between a ConstTag without a tag hole and an
   ArgTag with a tag hole merges them together.

   ... 1 +<| ?( _ ) ...  ==>  ... 1|( _ ) ...
   ... 1 |>+ ?( _ ) ...  ==>  ... 1|( _ ) ...
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (
          S(ConstTag(Tag(_, _) as tag), prefix),
          S(ArgTag(EmptyTagHole(_), ty), suffix),
        ),
      ),
    )
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (
          S(ConstTag(Tag(_, _) as tag), prefix),
          S(ArgTag(EmptyTagHole(_), ty), suffix),
        ),
      ),
    ) =>
    let zoperand = ZTyp.ArgTagZT(ZTag.place_after(tag), ty);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Backspace> after a sum body operator after a hole at the front of a
   sum body destroys the hole.

   ? +<| _ ...  ==>  |_ ...
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (S(ConstTag(EmptyTagHole(_)), E as prefix), S(operand, suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_before_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Backspace> after a sum body operator after a hole in any other
   position of a sum body destroys the hole.

   ... + ? +<| _ ...  ==>  ... +| _ ...
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (S(ConstTag(EmptyTagHole(_)), A(op, prefix)), suffix),
      ),
    ) =>
    switch (ZTyp.place_after_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Delete> before a sum body operator before a hole at the back of a
   sum body destroys the hole.

   ... _ |>+ ?  ==>  ... _|
   */
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (S(operand, prefix), S(ConstTag(EmptyTagHole(_)), E as suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_after_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Delete> before a sum body operator before a hole in any other
   position of a sum body destroys the hole.

   ... _ |>+ ? + ...  ==>  ... _ |+ ...
   */
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (prefix, S(ConstTag(EmptyTagHole(_)), A(op, suffix))),
      ),
    ) =>
    switch (ZTyp.place_before_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Delete> before a sum body operator before a non-hole and after a
   hole at the front of a sum body destroys the hole.

   ? |>+ 2 ...  ==>  |2 ...
   */
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (S(ConstTag(EmptyTagHole(_)), E as prefix), S(operand, suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_before_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Delete> before a sum body operator before a non-hole and after a
   hole at the back of a sum body destroys the hole.

   ... + ? |>+ 2 ...  ==>  ... |+ 2 ...
   */
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (S(ConstTag(EmptyTagHole(_)), A(op, prefix)), suffix),
      ),
    ) =>
    switch (ZTyp.place_before_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Backspace> after a sum body operator before a hole at the back of a
   sum body and after a non-hole destroys the hole.

   ... 1 +<| ?  ==>  ... 1|
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (S(operand, prefix), S(ConstTag(EmptyTagHole(_)), E as suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_after_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Backspace> before a hole and after a non-hole in any other position
   of a sum body destroys the hole.

   ... 1 +<| ? + ...  ==>  ... 1 +| ...
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (prefix, S(ConstTag(EmptyTagHole(_)), A(op, suffix))),
      ),
    ) =>
    switch (ZTyp.place_after_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Delete> before a sum body operator between two non-hole sum body
   operands at the back of a sum body destroys the right-hand operand.

   ... 1 |>+ 2  ==>  ... 1|
   */
  | (
      Delete,
      ZOperator(
        (OnOp(Before), _),
        (S(operand, prefix), S(_, E as suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_after_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Delete> before a sum body operator between two non-hole sum body
   operands at any other position of a sum body destroys the right-hand operand.

   ... 1 |>+ 2 + ...  ==>  ... 1 |+ ...
   */
  | (
      Delete,
      ZOperator((OnOp(Before), _), (prefix, S(_, A(op, suffix)))),
    ) =>
    switch (ZTyp.place_before_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /*
   Pressing <Backspace> after a sum body operator between two non-holes at the
   front of a sum body destroys the left-hand operand.

   1 +<| 2 ...  ==>  |2 ...
   */
  | (
      Backspace,
      ZOperator(
        (OnOp(After), _),
        (S(_, E as prefix), S(operand, suffix)),
      ),
    ) =>
    let zoperand = ZTyp.place_before_sumbody_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));

  /*
   Pressing <Backspace> after a sum body operator between two non-hole sum body
   operands at any other position of a sum body destroys the left-hand operand.

   ... + 1 +<| 2 ...  ==>  ... +| 2 ...
   */
  | (
      Backspace,
      ZOperator((OnOp(After), _), (S(_, A(op, prefix)), suffix)),
    ) =>
    switch (ZTyp.place_after_sumbody_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(zseq), u_gen));
    }

  /* Swapping */

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
      SwapRight,
      ZOperand(
        (CursorArgTag(_) | ConstTagZ(_) | ArgTagZT(_, _)) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));

  | (SwapLeft | SwapRight, ZOperand(ArgTagZA(tag, zty), surround)) =>
    switch (perform(u_gen, a, zty)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, u_gen)) =>
      let new_zseq = ZSeq.ZOperand(ZTyp.ArgTagZA(tag, zty), surround);
      Succeeded((ZTyp.mk_sumbody_ZOpSeq(new_zseq), u_gen));
    }

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

  /* Invalid actions on ArgTag delimiters */
  | (
      Construct(SOp(_) | SChar(_) | SList | SParenthesized),
      CursorArgTag(_, _, _),
    ) =>
    Failed

  /* Invalid swapping actions */
  | (SwapLeft | SwapRight, CursorArgTag(_, _, _)) => Failed

  /* Invalid cursor positions */
  | (_, CursorArgTag(OnText(_) | OnOp(_), _, _)) => Failed
  | (_, CursorArgTag(cursor, tag, ty))
      when !ZTyp.is_valid_cursor_sumbody_operand(cursor, ArgTag(tag, ty)) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumbody(u_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* _<|( _ )   ==>   _|( _ ) */
  /* _( _ <|)   ==>   _( _| ) */
  | (Backspace, CursorArgTag(OnDelim(_, Before), _, _)) =>
    ZTyp.is_before_zsumbody_operand(zoperand)
      ? CursorEscaped(Before)
      : perform_zsumbody_operand(u_gen, MoveLeft, zoperand)

  /* _(|> _ )   ==>   _( |_ ) */
  | (Delete, CursorArgTag(OnDelim(_, After), _, _)) =>
    ZTyp.is_after_zsumbody_operand(zoperand)
      ? CursorEscaped(After)
      : perform_zsumbody_operand(u_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  /* _|>( _ )  ==>  _(<| _ ) */
  /* _( _ |>)  ==>  _( _ )<| */
  | (Delete, CursorArgTag(OnDelim(k, Before), tag, ty)) =>
    perform_zsumbody_operand(
      u_gen,
      Backspace,
      CursorArgTag(OnDelim(k, After), tag, ty),
    )

  /* _( _ )<|   ==>   _| */
  /* _(<| _ )   ==>   _| */
  | (Backspace, CursorArgTag(OnDelim(_, After), tag, _)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.place_after_sumbody_operand(ConstTag(tag))),
      u_gen,
    ))

  /* Construction */

  // _|_  ==>  _ ( |? )
  | (Construct(SParenthesized), ConstTagZ(ztag)) =>
    let zty = ZTyp.place_before(UHTyp.mk_OpSeq(Seq.wrap(UHTyp.Hole)));
    let zoperand = ZTyp.ArgTagZA(ZTag.erase(ztag), zty);
    Succeeded((ZOpSeq.wrap(zoperand), u_gen));

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
