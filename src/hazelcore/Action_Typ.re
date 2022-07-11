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

let sum_body_operator_of_shape =
    (os: Action.operator_shape): option(UHTyp.sum_body_operator) =>
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

let shape_of_sum_body_operator =
    (op: UHTyp.sum_body_operator): Action.operator_shape =>
  switch (op) {
  | Plus => SPlus
  };

let fix_holes = (zty: ZTyp.t, id_gen: IDGen.t): (ZTyp.t, IDGen.t) => {
  let path = CursorPath_Typ.of_z(zty);
  let (ty, id_gen) = UHTyp.fix_holes(ZTyp.erase(zty), id_gen);
  let zty =
    CursorPath_Typ.follow(path, ty)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zty, id_gen);
};

let mk_and_fix_ZOpSeq = (zseq: ZTyp.zseq, id_gen: IDGen.t): (ZTyp.t, IDGen.t) => {
  let zopseq = ZTyp.mk_ZOpSeq(zseq);
  fix_holes(zopseq, id_gen);
};

let fix_holes_sum_body =
    (zsum_body: ZTyp.zsum_body, id_gen: IDGen.t): (ZTyp.zsum_body, IDGen.t) => {
  let path = CursorPath_Typ.of_zsum_body(zsum_body);
  let (sum_body, id_gen) =
    UHTyp.fix_holes_sum_body(ZTyp.erase_zsum_body(zsum_body), id_gen);
  let zsum_body =
    CursorPath_Typ.follow_sum_body(path, sum_body)
    |> OptUtil.get(() =>
         failwith(
           "fix_holes_sum_body did not preserve path "
           ++ Sexplib.Sexp.to_string(CursorPath.sexp_of_t(path)),
         )
       );
  (zsum_body, id_gen);
};
let mk_and_fix_sum_body_ZOpSeq =
    (zseq: ZTyp.sum_body_zseq, id_gen: IDGen.t): (ZTyp.zsum_body, IDGen.t) => {
  let zsum_body = ZTyp.mk_sum_body_ZOpSeq(zseq);
  fix_holes_sum_body(zsum_body, id_gen);
};

let construct_operator =
    (
      id_gen: IDGen.t,
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
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
  let zseq = ZSeq.ZOperand(zoperand, surround);
  mk_and_fix_ZOpSeq(zseq, id_gen);
};

let construct_zsum_body_operator =
    (
      id_gen: IDGen.t,
      operator: UHTyp.sum_body_operator,
      zoperand: ZTyp.zsum_body_operand,
      (prefix, suffix): ZTyp.sum_body_operand_surround,
    )
    : (ZTyp.zsum_body, IDGen.t) => {
  let (tag, id_gen) = UHTag.new_TagHole(id_gen);
  let ztag = ZTyp.ConstTagZ(ZTag.place_before(tag));
  let operand = zoperand |> ZTyp.erase_zsum_body_operand;
  let surround =
    if (ZTyp.is_before_zsum_body_operand(zoperand)) {
      (prefix, Seq.A(operator, S(operand, suffix)));
    } else {
      (Seq.A(operator, S(operand, prefix)), suffix);
    };
  (ZTyp.mk_sum_body_ZOpSeq(ZOperand(ztag, surround)), id_gen);
};

let rec move =
        (id_gen: IDGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, IDGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded((zty, id_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(id_gen, MoveTo(path), zty)
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
      | Some(path) => move(id_gen, MoveTo(path), zty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, id_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, id_gen))
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
  }

and move_zsum_body =
    (id_gen: IDGen.t, a: Action.t, zsum_body: ZTyp.zsum_body)
    : ActionOutcome.t((ZTyp.zsum_body, IDGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (
      CursorPath_Typ.follow_sum_body(path, zsum_body |> ZTyp.erase_zsum_body)
    ) {
    | None => Failed
    | Some(zsum_body) => Succeeded((zsum_body, id_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.prev_hole_steps(
        CursorPath_Typ.holes_zsum_body(zsum_body, []),
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sum_body(
          steps,
          zsum_body |> ZTyp.erase_zsum_body,
        )
      ) {
      | None => Failed
      | Some(path) => move_zsum_body(id_gen, MoveTo(path), zsum_body)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(
        next_hole_steps(CursorPath_Typ.holes_zsum_body(zsum_body, []))
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sum_body(
          steps,
          zsum_body |> ZTyp.erase_zsum_body,
        )
      ) {
      | None => Failed
      | Some(path) => move_zsum_body(id_gen, MoveTo(path), zsum_body)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left_zsum_body(zsum_body)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, id_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right_zsum_body(zsum_body)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, id_gen))
    }
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
        (id_gen: IDGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, IDGen.t)) =>
  perform_opseq(id_gen, a, zty)

and perform_opseq =
    (id_gen: IDGen.t, a: Action.t, ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq)
    : ActionOutcome.t((ZTyp.t, IDGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions at the type level */
  | (
      Construct(SAnn | SLet | SLine | SFun | SListNil | SInj | SMatch) | SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(id_gen, a, zopseq)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_opseq(id_gen, Action_common.escape(side), zopseq)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_opseq(
      id_gen,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    )

  // ..., [k-2], [k-1],<| [k], ...   ==>   ..., [k-2], [k-1]|, ...
  | (
      Backspace,
      ZOperator((OnOp(After), _), (S(operand, prefix), S(_, suffix))),
    ) =>
    let zoperand = ZTyp.place_after_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /* Construction */

  /*
   Pressing <Space> after an operator moves the cursor forward.

   ...,| _ ...  =( )=>  ..., |_ ...
   */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(id_gen, MoveRight, zopseq)

  /*
   When the cursor is on an operator, the cursor moves before construction.

   ...,| _ ...  =(,)=>  ..., |_ ...  =(,)=>  ..., ?,| _ ...
   ... _|, ...  =(,)=>  ... _|, ...  =(,)=>  ... _,| ?, ...
   */
  | (Construct(_), ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(id_gen, Action_common.escape(side), zopseq)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, id_gen)) => perform(id_gen, a, zty)
    }

  /*
   Pressing <Space> after an operand moves the cursor forward.

   ... _| ...  =( )=>  ... _ |...
   */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(id_gen, MoveRight, zopseq)

  /*
   Pressing <Space> inside an empty sum body constructs an empty tag hole.

   sum {| }  =( )=>  sum { |? }
   sum { |}  =( )=>  sum { ?| }
   */
  | (
      Construct(SOp(SSpace)),
      ZOperand(
        CursorT(OnDelim(0, After) | OnDelim(1, Before), FiniteSum(None)) as zoperand,
        surround,
      ),
    ) =>
    switch (perform_operand(id_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(id_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), id_gen)) =>
      Succeeded(mk_and_fix_ZOpSeq(ZSeq.insert(zseq, surround), id_gen))
    }

  | (Construct(SOp(SPlus)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (perform_operand(id_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(id_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), id_gen)) =>
      Succeeded(mk_and_fix_ZOpSeq(ZSeq.insert(zseq, surround), id_gen))
    }

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(construct_operator(id_gen, op, zoperand, surround))
    }

  /* SwapLeft and SwapRight are handled at block level */

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(op, S(operand, prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(op, S(operand, suffix));
    let zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  | (SwapRight, ZOperand(CursorT(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(op, S(operand, suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(op, S(operand, prefix));
    let zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, surround)) =>
    switch (perform_operand(id_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(id_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), id_gen)) =>
      let zseq = ZSeq.insert(zseq, surround);
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_operand =
    (id_gen: IDGen.t, a: Action.t, zoperand: ZTyp.zoperand)
    : ActionOutcome.t((ZTyp.t, IDGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions */
  | (
      Construct(
        SAnn | SLet | SLine | SFun | SListNil | SInj | SMatch | SCommentLine,
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
      CursorT(_, FiniteSum(_) | ElidedSum(_)),
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(id_gen, a, ZOpSeq.wrap(zoperand))

  /*
   Pressing <Space> in an empty sum body constructs a tag hole.

   sum {| }  =( )=>  sum { |? }
   sum { |}  =( )=>  sum { ?| }
   */
  | (
      Construct(SOp(SSpace)),
      CursorT(
        OnDelim(0 as j, After) | OnDelim(1 as j, Before),
        FiniteSum(None),
      ),
    ) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    let place_cursor = ZTag.(j == 0 ? place_before : place_after);
    let ztag = ZTyp.ConstTagZ(place_cursor(tag));
    let zseq = ZSeq.wrap(ZTyp.FiniteSumZ(ZOpSeq.wrap(ztag)));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /*
   Pressing <Plus> inside a sum body delimiter wraps the sum in another sum.

   sum {| ... }  =(+)=>  sum { |?( sum { ... } ) }
   sum { ... |}  =(+)=>  sum { |?( sum { ... } ) }
   */
  | (
      Construct(SOp(SPlus)),
      CursorT(
        OnDelim(0, After) | OnDelim(1, Before),
        FiniteSum(_) as operand,
      ),
    ) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    switch (ZTag.place_cursor(OnDelim(0, Before), tag)) {
    | None => Failed
    | Some(ztag) =>
      let zoperand = ZTyp.ArgTagZT(ztag, OpSeq.wrap(operand));
      let zseq = ZSeq.wrap(ZTyp.FiniteSumZ(ZOpSeq.wrap(zoperand)));
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
    };
  | (
      Construct(SOp(SPlus)),
      CursorT(
        OnDelim(0, After) | OnDelim(1, Before),
        ElidedSum(_) as operand,
      ),
    ) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    switch (ZTag.place_cursor(OnDelim(0, Before), tag)) {
    | None => Failed
    | Some(ztag) =>
      let zoperand = ZTyp.ArgTagZT(ztag, OpSeq.wrap(operand));
      let zseq = ZSeq.wrap(ZTyp.ElidedSumZ(zoperand));
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
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
        FiniteSum(_) as operand,
      ),
    ) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    let sum_body = OpSeq.wrap(UHTyp.ArgTag(tag, OpSeq.wrap(operand)));
    switch (
      ZTyp.place_cursor_operand(cursor, UHTyp.FiniteSum(Some(sum_body)))
    ) {
    | None => Failed
    | Some(zoperand) =>
      Succeeded(mk_and_fix_ZOpSeq(ZSeq.wrap(zoperand), id_gen))
    };
  | (
      Construct(SOp(SPlus)),
      CursorT(
        (OnDelim(0, Before) | OnDelim(1, After)) as cursor,
        ElidedSum(_) as operand,
      ),
    ) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    let operand = UHTyp.ArgTag(tag, OpSeq.wrap(operand));
    switch (ZTyp.place_cursor_operand(cursor, UHTyp.ElidedSum(operand))) {
    | None => Failed
    | Some(zoperand) =>
      Succeeded(mk_and_fix_ZOpSeq(ZSeq.wrap(zoperand), id_gen))
    };

  /*
   Pressing a valid tag character inside an empty sum constructs a tag.

   sum { | }  =(c)=>  sum { c| }
   */
  | (
      Construct(SChar(c)),
      CursorT(OnDelim(0, After) | OnDelim(1, Before), FiniteSum(None)),
    ) =>
    let (ztag, id_gen) = Action_Tag.mk_text(0, c, id_gen);
    let zseq =
      ZSeq.wrap(ZTyp.FiniteSumZ(ZOpSeq.wrap(ZTyp.ConstTagZ(ztag))));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /*
   Pressing <Plus> on a type hole constructs a tag hole inside a sum.

   |?  ==>  sum { |? }
   ?|  ==>  sum { |? }
   */
  | (Construct(SOp(SPlus)), CursorT(OnDelim(_, _), Hole)) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    switch (ZTag.place_cursor(OnDelim(0, Before), tag)) {
    | None => Failed
    | Some(ztag) =>
      let zsum_body = ZOpSeq.wrap(ZTyp.ConstTagZ(ztag));
      let zseq = ZSeq.wrap(ZTyp.FiniteSumZ(zsum_body));
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
    };

  /*
   Pressing <Plus> on a type constructs an ArgTag, with the type as its
   argument, inside a sum.

   |_  ==>  sum { ?( |_ ) }
   _|  ==>  sum { ?( _| ) }
   */
  | (Construct(SOp(SPlus)), CursorT(_, _)) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    let zsum_body = ZOpSeq.wrap(ZTyp.ArgTagZA(tag, ZOpSeq.wrap(zoperand)));
    let zseq = ZSeq.wrap(ZTyp.FiniteSumZ(zsum_body));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /*
   Destroying a sum body delimiter from the outside destroys the sum.

   |>sum { ... }  ==>  |?
   sum { ... }<|  ==>  ?|
   */
  | (Delete, CursorT(OnDelim(0 as j, Before), FiniteSum(_) | ElidedSum(_)))
  | (
      Backspace,
      CursorT(OnDelim(1 as j, After), FiniteSum(_) | ElidedSum(_)),
    ) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_operand : ZTyp.place_after_operand;
    let zseq = ZSeq.wrap(place_cursor(UHTyp.Hole));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /*
   Destroying the inside of a sum body delimiter destroys the sum.

   sum { ... |>}  ==>  ?|
   sum {<| ... }  ==>  |?
   */
  | (Backspace, CursorT(OnDelim(j, After), FiniteSum(_) | ElidedSum(_)))
  | (Delete, CursorT(OnDelim(j, Before), FiniteSum(_) | ElidedSum(_))) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_operand : ZTyp.place_after_operand;
    let zseq = ZSeq.wrap(place_cursor(UHTyp.Hole));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /*
   Destroying a singleton tag hole destroys the sum body.
   */
  // sum { |>? }  ==>  sum { |}
  // sum { ?<| }  ==>  sum {| }
  | (
      Delete,
      FiniteSumZ(
        ZOpSeq(
          _,
          ZOperand(
            ConstTagZ(
              CursorTag(OnDelim(_0, Before as side), EmptyTagHole(_)),
            ),
            (E, E),
          ),
        ),
      ) |
      ElidedSumZ(
        ConstTagZ(CursorTag(OnDelim(_0, Before as side), EmptyTagHole(_))),
      ),
    )
  | (
      Backspace,
      FiniteSumZ(
        ZOpSeq(
          _,
          ZOperand(
            ConstTagZ(
              CursorTag(OnDelim(_0, After as side), EmptyTagHole(_)),
            ),
            (E, E),
          ),
        ),
      ) |
      ElidedSumZ(
        ConstTagZ(CursorTag(OnDelim(_0, After as side), EmptyTagHole(_))),
      ),
    ) =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_cursor_operand(OnDelim(1, Before))
      | After => ZTyp.place_cursor_operand(OnDelim(0, After))
      };
    switch (place_cursor(FiniteSum(None))) {
    | None => Failed
    | Some(zoperand) =>
      Succeeded(mk_and_fix_ZOpSeq(ZSeq.wrap(zoperand), id_gen))
    };

  /*
   When a destruction action is toward a sum body delimiter, the action becomes
   movement.
   */
  // sum {|> }  ==>  sum { |}
  // sum { <|}  ==>  sum {| }
  | (Delete, CursorT(OnDelim(0 as j, After), FiniteSum(None) as operand))
  | (
      Backspace,
      CursorT(OnDelim(1 as j, Before), FiniteSum(None) as operand),
    ) =>
    let place_cursor =
      j == 0
        ? ZTyp.place_cursor_operand(OnDelim(1, Before))
        : ZTyp.place_cursor_operand(OnDelim(0, After));
    switch (place_cursor(operand)) {
    | None => Failed
    | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), id_gen))
    };

  /*
   When a destructive action is toward a sum body, the action becomes movement.

   sum {|> _ ... }  ==>  sum { |_ ... }
   sum { ... _ <|}  ==>  sum { ... _| }
   */
  | (Delete, CursorT(OnDelim(0 as j, After), FiniteSum(Some(sum_body))))
  | (
      Backspace,
      CursorT(OnDelim(1 as j, Before), FiniteSum(Some(sum_body))),
    ) =>
    let place_cursor =
      j == 0 ? ZTyp.place_before_sum_body : ZTyp.place_after_sum_body;
    let zty = ZOpSeq.wrap(ZTyp.FiniteSumZ(place_cursor(sum_body)));
    Succeeded((zty, id_gen));

  /*
   When a destruction action is away from a sum, then action escapes.

   sum { ... }|>  ==>  CursorEscaped(After)
   <|sum { ... }  ==>  CursorEscaped(Before)
   */
  | (
      Delete,
      CursorT(OnDelim(1, After as side), FiniteSum(_) | ElidedSum(_)),
    )
  | (
      Backspace,
      CursorT(OnDelim(0, Before as side), FiniteSum(_) | ElidedSum(_)),
    ) =>
    CursorEscaped(side)

  /* Zippered Sums */
  | (_, FiniteSumZ(zsum_body)) =>
    switch (perform_zsum_body(id_gen, a, zsum_body)) {
    | Failed as failed => failed
    | Succeeded((zsum_body, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.FiniteSumZ(zsum_body)), id_gen))
    | CursorEscaped(side) =>
      let place_cursor =
        switch (side) {
        | Before => ZTyp.place_cursor_operand(OnDelim(0, After))
        | After => ZTyp.place_cursor_operand(OnDelim(1, Before))
        };
      switch (
        place_cursor(
          UHTyp.FiniteSum(Some(ZTyp.erase_zsum_body(zsum_body))),
        )
      ) {
      | None => Failed
      | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), id_gen))
      };
    }
  | (_, ElidedSumZ(zoperand)) =>
    switch (perform_zsum_body_operand(id_gen, a, zoperand)) {
    | Failed as failed => failed
    | CursorEscaped(side) =>
      let place_cursor =
        switch (side) {
        | Before => ZTyp.place_cursor_operand(OnDelim(0, After))
        | After => ZTyp.place_cursor_operand(OnDelim(1, Before))
        };
      let zoperand_opt = ZTyp.erase_zsum_body_operand(zoperand);
      switch (place_cursor(UHTyp.ElidedSum(zoperand_opt))) {
      | None => Failed
      | Some(zoperand) => Succeeded((ZOpSeq.wrap(zoperand), id_gen))
      };
    | Succeeded((ZOpSeq(_, ZOperand(zoperand', (E, E))), id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ElidedSumZ(zoperand')), id_gen))
    | Succeeded(_) => Failed
    }

  /* -- End of Sums -- */

  /* Backspace and Delete */

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before) : perform_operand(id_gen, MoveLeft, zoperand)

  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After) : perform_operand(id_gen, MoveRight, zoperand)

  | (Delete, CursorT(OnDelim(_, Before), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Hole)), id_gen))

  | (Delete, CursorT(OnDelim(_, Before), Unit | Int | Float | Bool)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Hole)), id_gen))

  | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), id_gen))

  | (Backspace, CursorT(OnDelim(_, After), Unit | Int | Float | Bool)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), id_gen))

  /* |>( _ )  ==>  |_ */
  /* ( _ |>)  ==>  _| */
  | (
      Delete,
      CursorT(OnDelim(k, Before), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    let ZOpSeq(_, zseq) = body |> place_cursor;
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /* ( _ )<|  ==>  _| */
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    let ZOpSeq(_, zseq) = body |> place_cursor;
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  /* Construction */

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_operand(id_gen, MoveRight, zoperand)

  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (perform_operand(id_gen, Action_common.escape(side), zoperand)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, id_gen)) => perform(id_gen, a, zty)
    }

  | (Construct(SChar("I")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Int)), id_gen))
  | (Construct(SChar("F")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Float)), id_gen))
  | (Construct(SChar("B")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Bool)), id_gen))
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))), id_gen))

  | (Construct(SCloseSquareBracket), ListZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), UHTyp.List(ZTyp.erase(zopseq))),
      ),
      id_gen,
    ))
  | (Construct(SCloseSquareBracket), CursorT(_, _)) => Failed

  | (Construct(SParenthesized), CursorT(_)) =>
    let zseq = ZSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand)));
    Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));

  | (Construct(SCloseBraces), CursorT(_)) => Failed

  | (Construct(SCloseParens), ParenthesizedZ(zopseq))
      when ZTyp.is_after(zopseq) =>
    Succeeded((
      ZOpSeq.wrap(
        ZTyp.CursorT(OnDelim(1, After), Parenthesized(ZTyp.erase(zopseq))),
      ),
      id_gen,
    ))
  | (
      Construct(SCloseParens),
      CursorT(OnDelim(1, Before), Parenthesized(opseq)),
    ) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.CursorT(OnDelim(1, After), Parenthesized(opseq))),
      id_gen,
    ))
  | (Construct(SCloseParens), CursorT(_, _)) => Failed
  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded(construct_operator(id_gen, op, zoperand, (E, E)))
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed

  /* Zipper Cases */

  | (_, ParenthesizedZ(zbody)) =>
    switch (perform(id_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(id_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, id_gen)) =>
      let zseq = ZSeq.wrap(ZTyp.ParenthesizedZ(zbody));
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
    }

  | (_, ListZ(zbody)) =>
    switch (perform(id_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(id_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, id_gen)) =>
      let zseq = ZSeq.wrap(ZTyp.ListZ(zbody));
      Succeeded(mk_and_fix_ZOpSeq(zseq, id_gen));
    }

  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_zsum_body =
    (
      id_gen: IDGen.t,
      a: Action.t,
      ZOpSeq(_, zseq) as zsum_body: ZTyp.zsum_body,
    )
    : ActionOutcome.t((ZTyp.zsum_body, IDGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions on sum body operators */
  | (
      Construct(
        SCommentLine | SList | SAnn | SFun | SListNil | SInj | SLet | SLine |
        SMatch |
        SParenthesized |
        SCloseParens |
        SCloseBraces |
        SCloseSquareBracket |
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
  | (SwapLeft, ZOperand(ConstTagZ(_) | ArgTagZT(_, _), (E, _)))
  | (SwapRight, ZOperand(ConstTagZ(_) | ArgTagZT(_, _), (_, E))) => Failed

  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement is handled separately */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsum_body(id_gen, a, zsum_body)

  /*
   Pressing <Plus> between a tag and its argument splits them apart.

   ... _|( _ ) ...  =(+)=>  ... _ +| ?( _ ) ...
   */
  | (
      Construct(SOp(SPlus)),
      ZOperand(ArgTagZT(ztag, ty), (prefix, suffix)),
    )
      when ZTag.is_after(ztag) =>
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let (tag, id_gen) = UHTag.new_TagHole(id_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(ZTag.erase(ztag)), prefix);
      let new_suffix = Seq.S(UHTyp.ArgTag(tag, ty), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let n = String.length(t);
      let (tag1, id_gen) = Action_Tag.mk_tag(String.sub(t, 0, j), id_gen);
      let (tag2, id_gen) =
        Action_Tag.mk_tag(String.sub(t, j, n - j), id_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(tag1), prefix);
      let new_suffix = Seq.S(UHTyp.ConstTag(tag2), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let n = String.length(t);
      let (tag1, id_gen) = Action_Tag.mk_tag(String.sub(t, 0, j), id_gen);
      let (tag2, id_gen) =
        Action_Tag.mk_tag(String.sub(t, j, n - j), id_gen);
      let new_prefix = Seq.S(UHTyp.ConstTag(tag1), prefix);
      let new_suffix = Seq.S(UHTyp.ArgTag(tag2, ty), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    }

  /*
   Pressing <Plus> before a sum body operand constructs a tag hole.

   ... |_ ...  =(+)=>  ... ? +| _ ...
   */
  | (Construct(SOp(SPlus)), ZOperand(zoperand, (prefix, suffix)))
      when ZTyp.is_before_zsum_body_operand(zoperand) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let new_prefix = Seq.S(UHTyp.ConstTag(tag), prefix);
      let new_suffix = Seq.S(ZTyp.erase_zsum_body_operand(zoperand), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    };

  /*
   Pressing <Plus> after a sum body operand constructs a tag hole.

   ... _| ...  =(+)=>  ... _ +| ? ...
   */
  | (Construct(SOp(SPlus)), ZOperand(zoperand, (prefix, suffix)))
      when ZTyp.is_after_zsum_body_operand(zoperand) =>
    let (tag, id_gen) = UHTag.new_TagHole(id_gen);
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let new_prefix = Seq.S(ZTyp.erase_zsum_body_operand(zoperand), prefix);
      let new_suffix = Seq.S(UHTyp.ConstTag(tag), suffix);
      let zseq = ZSeq.ZOperator(zop, (new_prefix, new_suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    switch (ZTyp.place_after_sum_body_operator(Operators_SumBody.Plus)) {
    | None => Failed
    | Some(zop) =>
      let (tag, id_gen) = UHTag.new_TagHole(id_gen);
      let surround =
        switch (side) {
        | Before => (prefix, Seq.S(UHTyp.ConstTag(tag), Seq.A(op, suffix)))
        | After => (Seq.S(UHTyp.ConstTag(tag), Seq.A(op, prefix)), suffix)
        };
      let zseq = ZSeq.ZOperator(zop, surround);
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    }

  /*
   Pressing <Plus> in an ArgTag body applies the action to the body.

   ... + _ ( |_ ) + ...  =(+)=>  ... + _ ( sum { |_ } ) + ...
   ... + _ ( _| ) + ...  =(+)=>  ... + _ ( sum { _| } ) + ...
   */
  | (Construct(SOp(SPlus)), ZOperand(ArgTagZA(tag, zty), surround)) =>
    switch (perform(id_gen, a, zty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsum_body(id_gen, Action_common.escape(side), zsum_body)
    | Succeeded((zty, id_gen)) =>
      let zseq = ZSeq.ZOperand(ZTyp.ArgTagZA(tag, zty), surround);
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    }

  /*
   Pressing <Space> on a sum body operator moves the cursor to the right.

   _ |+ _  =( )=>  _ +| _
   _ +| _  =( )=>  _ + |_
   */
  | (Construct(SOp(SSpace)), ZOperator(_, _)) =>
    perform_zsum_body(id_gen, MoveRight, zsum_body)

  /*
   Pressing a valid tag character before a sum body operator redirects the
   action to the left operand.

   ... 1 |+ ...  =(c)=>  ... 1| + ...  =(c)=>  ... 1c| + ...
   */
  | (Construct(SChar(c)), ZOperator((OnOp(side), _), _))
      when UHTag.is_tag_char(c.[0]) =>
    switch (perform_zsum_body(id_gen, Action_common.escape(side), zsum_body)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zsum_body, id_gen)) =>
      perform_zsum_body(id_gen, a, zsum_body)
    }

  /*
   Pressing a valid tag character after a sum body operator redirects the action
   to the right operand.

   ... 1 |+ ...  =(c)=>  ... 1| + ...  =(c)=>  ... 1c| + ...
   */
  | (Construct(SChar(c)), ZOperator((OnOp(side), _), _))
      when UHTag.is_tag_char(c.[0]) =>
    switch (perform_zsum_body(id_gen, Action_common.escape(side), zsum_body)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zsum_body, id_gen)) =>
      perform_zsum_body(id_gen, a, zsum_body)
    }

  | (Construct(SChar(_)), ZOperator(_, _)) => Failed

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
    move_zsum_body(id_gen, MoveRight, zsum_body)

  /*
   Pressing <Backspace> before a sum body operator moves the cursor left.

   ... _ <|+ _ ...  ==>  ... _| + _ ...
   */
  | (Backspace, ZOperator((OnOp(Before), _), _)) =>
    move_zsum_body(id_gen, MoveLeft, zsum_body)

  /*
   Pressing <Delete> after a sum body operand moves the cursor right.

   ... _|> + _ ...  ==>  ... _ |+ _ ...
   */
  | (Delete, ZOperand(zoperand, _))
      when ZTyp.is_after_zsum_body_operand(zoperand) =>
    move_zsum_body(id_gen, MoveRight, zsum_body)

  /*
   Pressing <Backspace> before a sum body operand moves the cursor left.

   ... _ + <|_ ...  ==>  ... _ +| _ ...
   */
  | (Backspace, ZOperand(zoperand, _))
      when ZTyp.is_before_zsum_body_operand(zoperand) =>
    move_zsum_body(id_gen, MoveLeft, zsum_body)

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
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

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
    let zoperand = ZTyp.place_before_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

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
    switch (ZTyp.place_after_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    let zoperand = ZTyp.place_after_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

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
    switch (ZTyp.place_before_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    let zoperand = ZTyp.place_before_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

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
    switch (ZTyp.place_before_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    let zoperand = ZTyp.place_after_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

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
    switch (ZTyp.place_after_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    let zoperand = ZTyp.place_after_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  /*
   Pressing <Delete> before a sum body operator between two non-hole sum body
   operands at any other position of a sum body destroys the right-hand operand.

   ... 1 |>+ 2 + ...  ==>  ... 1 |+ ...
   */
  | (
      Delete,
      ZOperator((OnOp(Before), _), (prefix, S(_, A(op, suffix)))),
    ) =>
    switch (ZTyp.place_before_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
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
    let zoperand = ZTyp.place_before_sum_body_operand(operand);
    let zseq = ZSeq.ZOperand(zoperand, (prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  /*
   Pressing <Backspace> after a sum body operator between two non-hole sum body
   operands at any other position of a sum body destroys the left-hand operand.

   ... + 1 +<| 2 ...  ==>  ... +| 2 ...
   */
  | (
      Backspace,
      ZOperator((OnOp(After), _), (S(_, A(op, prefix)), suffix)),
    ) =>
    switch (ZTyp.place_after_sum_body_operator(op)) {
    | None => Failed
    | Some(zop) =>
      let zseq = ZSeq.ZOperator(zop, (prefix, suffix));
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    }

  /* Swapping */

  | (
      SwapLeft,
      ZOperand(
        (CursorArgTag(_) | ConstTagZ(_) | ArgTagZT(_, _)) as zoperand,
        (A(operator, S(operand, prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let zseq = ZSeq.ZOperand(zoperand, (prefix, new_suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  | (
      SwapRight,
      ZOperand(
        (CursorArgTag(_) | ConstTagZ(_) | ArgTagZT(_, _)) as zoperand,
        (prefix, A(op, S(operand, suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(op, S(operand, prefix));
    let zseq = ZSeq.ZOperand(zoperand, (new_prefix, suffix));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  | (SwapLeft | SwapRight, ZOperand(ArgTagZA(tag, zty), surround)) =>
    switch (perform(id_gen, a, zty)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, id_gen)) =>
      let zseq = ZSeq.ZOperand(ZTyp.ArgTagZA(tag, zty), surround);
      Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
    }

  /* Zipper */

  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_zsum_body_operand(id_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsum_body(id_gen, Action_common.escape(side), zsum_body)
    | Succeeded((ZOpSeq(_, zseq), id_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        let zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
        Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        let zseq = ZSeq.ZOperator(zoperator, (new_prefix, new_suffix));
        Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }

and perform_zsum_body_operand =
    (id_gen: IDGen.t, a: Action.t, zoperand: ZTyp.zsum_body_operand)
    : ActionOutcome.t((ZTyp.zsum_body, IDGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions */
  | (
      Construct(
        SAnn | SLet | SLine | SFun | SListNil | SInj | SMatch | SOp(SPlus) |
        SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Invalid actions on ArgTag delimiters */
  | (
      Construct(
        SOp(_) | SChar(_) | SList | SParenthesized | SCloseBraces |
        SCloseSquareBracket,
      ),
      CursorArgTag(_, _, _),
    ) =>
    Failed

  /* Invalid swapping actions */
  | (SwapLeft | SwapRight, CursorArgTag(_, _, _)) => Failed

  /* Invalid cursor positions */
  | (_, CursorArgTag(OnText(_) | OnOp(_), _, _)) => Failed
  | (_, CursorArgTag(cursor, tag, ty))
      when !ZTyp.is_valid_cursor_sum_body_operand(cursor, ArgTag(tag, ty)) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsum_body(id_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* _<|( _ )   ==>   _|( _ ) */
  /* _( _ <|)   ==>   _( _| ) */
  | (Backspace, CursorArgTag(OnDelim(_, Before), _, _)) =>
    ZTyp.is_before_zsum_body_operand(zoperand)
      ? CursorEscaped(Before)
      : perform_zsum_body_operand(id_gen, MoveLeft, zoperand)

  /* _(|> _ )   ==>   _( |_ ) */
  | (Delete, CursorArgTag(OnDelim(_, After), _, _)) =>
    ZTyp.is_after_zsum_body_operand(zoperand)
      ? CursorEscaped(After)
      : perform_zsum_body_operand(id_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  /* _|>( _ )  ==>  _(<| _ ) */
  /* _( _ |>)  ==>  _( _ )<| */
  | (Delete, CursorArgTag(OnDelim(k, Before), tag, ty)) =>
    perform_zsum_body_operand(
      id_gen,
      Backspace,
      CursorArgTag(OnDelim(k, After), tag, ty),
    )

  /* _( _ )<|   ==>   _| */
  /* _(<| _ )   ==>   _| */
  | (Backspace, CursorArgTag(OnDelim(_, After), tag, _)) =>
    let zseq = ZSeq.wrap(ZTyp.place_after_sum_body_operand(ConstTag(tag)));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  /* Construction */

  // _|_  ==>  _ ( |? )
  | (Construct(SParenthesized), ConstTagZ(ztag)) =>
    let zty = ZTyp.place_before(UHTyp.mk_OpSeq(Seq.wrap(UHTyp.Hole)));
    let zseq = ZSeq.wrap(ZTyp.ArgTagZA(ZTag.erase(ztag), zty));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  // _( _ |)  ==>  _( _ )|
  | (Construct(SCloseParens), CursorArgTag(OnDelim(1, Before), _, _)) =>
    let operand = ZTyp.erase_zsum_body_operand(zoperand);
    let zseq = ZSeq.wrap(ZTyp.place_after_sum_body_operand(operand));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));
  | (Construct(SCloseParens), CursorArgTag(_)) => Failed

  // _( _| )  ==>  _( _ )|
  | (Construct(SCloseParens), ArgTagZA(_, zty)) when ZTyp.is_after(zty) =>
    let operand = ZTyp.erase_zsum_body_operand(zoperand);
    let zseq = ZSeq.wrap(ZTyp.place_after_sum_body_operand(operand));
    Succeeded(mk_and_fix_sum_body_ZOpSeq(zseq, id_gen));

  /* Zipper Cases */

  | (_, ConstTagZ(ztag)) =>
    switch (Action_Tag.perform(id_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsum_body_operand(id_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)), id_gen))
    }

  | (_, ArgTagZT(ztag, ty)) =>
    switch (Action_Tag.perform(id_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsum_body_operand(id_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZT(ztag, ty)), id_gen))
    }

  | (_, ArgTagZA(tag, zty)) =>
    switch (perform(id_gen, a, zty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsum_body_operand(id_gen, Action_common.escape(side), zoperand)
    | Succeeded((zty, id_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZA(tag, zty)), id_gen))
    }

  | (Init, _) => failwith("Init action should not be performed.")
  };
