let text_operand = (shape: TyTextShape.t, id_gen: IDGen.t): (TPat.t, IDGen.t) => {
  switch (shape) {
  | Int =>
    let (u, id_gen) = IDGen.next_hole(id_gen);
    (TyVar(InHole(BuiltinType, u), "Int"), id_gen);
  | Bool =>
    let (u, id_gen) = IDGen.next_hole(id_gen);
    (TyVar(InHole(BuiltinType, u), "Bool"), id_gen);
  | Float =>
    let (u, id_gen) = IDGen.next_hole(id_gen);
    (TyVar(InHole(BuiltinType, u), "Float"), id_gen);
  | ExpandingKeyword(kw) =>
    let (u, id_gen) = IDGen.next_hole(id_gen);
    let name = ExpandingKeyword.to_string(kw);
    (TyVar(InHole(ReservedKeyword, u), name), id_gen);
  | TyVar(name) => (TyVar(NotInHole, name), id_gen)
  };
};

let rec move = (a: Action.t, ztp: ZTPat.t): ActionOutcome.t(ZTPat.t) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_TPat.follow(path, ZTPat.erase(ztp))) {
    | None => Failed
    | Some(ztp) => Succeeded(ztp)
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_TPat.holes_z(ztp, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_TPat.of_steps(steps, ZTPat.erase(ztp))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), ztp)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_TPat.holes_z(ztp, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_TPat.of_steps(steps, ZTPat.erase(ztp))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), ztp)
      }
    }
  | MoveLeft =>
    switch (ZTPat.move_cursor_left(ztp)) {
    | None => Failed
    | Some(ztp) => Succeeded(ztp)
    }
  | MoveRight =>
    switch (ZTPat.move_cursor_right(ztp)) {
    | None => Failed
    | Some(ztp) => Succeeded(ztp)
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
        (a: Action.t, ztp: ZTPat.t, id_gen: IDGen.t)
        : ActionOutcome.t((ZTPat.t, IDGen.t)) =>
  switch (a, ztp) {
  | (
      Construct(
        SCloseParens | SCloseBraces | SCloseSquareBracket | SCommentLine | SAnn |
        SFun |
        SLet |
        SLine |
        SListNil |
        SInj(_) |
        SCase |
        STyAlias |
        SList |
        SParenthesized,
      ) |
      SwapUp |
      SwapDown |
      SwapLeft |
      SwapRight,
      _,
    ) =>
    Failed

  | (_, CursorP(OnOp(_), _)) => Failed
  | (_, CursorP(cursor, tp)) when !ZTPat.is_valid_cursor(cursor, tp) =>
    Failed

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    switch (move(a, ztp)) {
    | Succeeded(ztp) => Succeeded((ztp, id_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }

  | (Backspace, CursorP(OnDelim(_, Before), _)) => CursorEscaped(Before)

  | (Delete, CursorP(OnDelim(_, After), _)) => CursorEscaped(After)

  | (Backspace, CursorP(OnDelim(k, After), EmptyHole)) =>
    Succeeded((CursorP(OnDelim(k, Before), EmptyHole), id_gen))

  | (Delete, CursorP(OnDelim(k, Before), EmptyHole)) =>
    Succeeded((CursorP(OnDelim(k, After), EmptyHole), id_gen))

  | (Backspace, CursorP(OnDelim(_, After), _)) => Failed

  | (Delete, CursorP(OnDelim(k, Before), tp)) =>
    perform(Backspace, CursorP(OnDelim(k, After), tp), id_gen)

  /* Backspace and delete on tyvar */
  | (Backspace, CursorP(OnText(k), TyVar(_, name) | InvalidText(_, name))) =>
    if (ZTPat.is_before(ztp)) {
      CursorEscaped(Before);
    } else {
      let new_name = StringUtil.backspace(k, name);
      if (StringUtil.is_empty(new_name)) {
        Succeeded((ZTPat.place_before(EmptyHole), id_gen));
      } else {
        let (_, tp, id_gen) =
          Statics_TPat.ana_fix_holes(
            InitialContext.ctx,
            TPat.of_string(new_name),
            Hole,
            id_gen,
          );
        Succeeded((CursorP(OnText(k - 1), tp), id_gen));
      };
    }

  | (Backspace, CursorP(OnText(_), _)) => Failed

  | (Delete, CursorP(OnText(k), TyVar(_, name) | InvalidText(_, name))) =>
    if (ZTPat.is_after(ztp)) {
      CursorEscaped(After);
    } else {
      let new_name = StringUtil.delete(k, name);
      if (StringUtil.is_empty(new_name)) {
        Succeeded((ZTPat.place_before(EmptyHole), id_gen));
      } else {
        let (_, tp, id_gen) =
          Statics_TPat.ana_fix_holes(
            InitialContext.ctx,
            TPat.of_string(new_name),
            Hole,
            id_gen,
          );
        Succeeded((CursorP(OnText(k), tp), id_gen));
      };
    }

  | (Delete, CursorP(OnText(_), _)) => Failed

  /* Construction */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
    perform(MoveRight, ztp, id_gen)

  | (Construct(SOp(_)), _) => Failed

  | (Construct(SChar(s)), CursorP(_, EmptyHole)) =>
    let (_, tp, id_gen) =
      Statics_TPat.ana_fix_holes(
        InitialContext.ctx,
        TyVar(NotInHole, s),
        Hole,
        id_gen,
      );
    Succeeded((CursorP(OnText(1), tp), id_gen));

  | (
      Construct(SChar(s)),
      CursorP(OnText(k), TyVar(_, t) | InvalidText(_, t)),
    ) =>
    let new_t = StringUtil.insert(k, s, t);
    let (_, tp, id_gen) =
      Statics_TPat.ana_fix_holes(
        InitialContext.ctx,
        TyVar(NotInHole, new_t),
        Hole,
        id_gen,
      );
    Succeeded((CursorP(OnText(k + 1), tp), id_gen));

  | (Construct(SChar(_)), _) => Failed
  | (Init, _) => failwith("Init action should not be performed")
  };
