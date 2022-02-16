let text_operand =
    (shape: TyTextShape.t, u_gen: MetaVarGen.t): (TPat.t, MetaVarGen.t) => {
  switch (shape) {
  | Int =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (TyVar(InHole(BuiltinType, u), "Int"), u_gen);
  | Bool =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (TyVar(InHole(BuiltinType, u), "Bool"), u_gen);
  | Float =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (TyVar(InHole(BuiltinType, u), "Float"), u_gen);
  | ExpandingKeyword(kw) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let name = ExpandingKeyword.to_string(kw);
    (TyVar(InHole(ReservedKeyword, u), name), u_gen);
  | TyVar(name) => (TyVar(NotInHole, name), u_gen)
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

let rec perform =
        (a: Action.t, ztp: ZTPat.t, u_gen: MetaVarGen.t)
        : ActionOutcome.t((ZTPat.t, MetaVarGen.t)) =>
  switch (a, ztp) {
  | (
      UpdateApPalette(_) |
      Construct(
        SCloseParens | SCloseBraces | SCloseSquareBracket | SCommentLine | SAnn |
        SLet |
        SLine |
        SLam |
        SListNil |
        SInj(_) |
        SCase |
        STyAlias |
        SApPalette(_) |
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
    | Succeeded(ztp) => Succeeded((ztp, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }

  | (Backspace, CursorP(OnDelim(_, Before), _)) => CursorEscaped(Before)

  | (Delete, CursorP(OnDelim(_, After), _)) => CursorEscaped(After)

  | (Backspace, CursorP(OnDelim(k, After), EmptyHole)) =>
    Succeeded((CursorP(OnDelim(k, Before), EmptyHole), u_gen))

  | (Backspace, CursorP(OnDelim(_, After), _)) => Failed

  | (Delete, CursorP(OnDelim(k, Before), tp)) =>
    perform(Backspace, CursorP(OnDelim(k, After), tp), u_gen)

  /* Backspace and delete on tyvar */
  | (Backspace, CursorP(OnText(k), TyVar(_, name))) =>
    if (ZTPat.is_before(ztp)) {
      CursorEscaped(Before);
    } else {
      let new_name = StringUtil.backspace(k, name);
      if (StringUtil.is_empty(new_name)) {
        Succeeded((ZTPat.place_after(EmptyHole), u_gen));
      } else {
        let tp = TPat.of_string(new_name);
        Succeeded((CursorP(OnText(k - 1), tp), u_gen));
      };
    }

  | (Backspace, CursorP(OnText(_), _)) => Failed

  | (Delete, CursorP(OnText(k), TyVar(_, name))) =>
    if (ZTPat.is_after(ztp)) {
      CursorEscaped(After);
    } else {
      let new_name = StringUtil.delete(k, name);
      if (StringUtil.is_empty(new_name)) {
        Succeeded((ZTPat.place_before(EmptyHole), u_gen));
      } else {
        let tp = TPat.of_string(new_name);
        Succeeded((CursorP(OnText(k - 1), tp), u_gen));
      };
    }

  | (Delete, CursorP(OnText(_), _)) => Failed
  /* Construction */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
    perform(MoveRight, ztp, u_gen)

  | (Construct(SOp(_)), _) => Failed

  | (Construct(SChar(s)), CursorP(_, EmptyHole)) =>
    let tp = TPat.of_string(s);
    Succeeded((CursorP(OnText(1), tp), u_gen));

  | (Construct(SChar(s)), CursorP(OnText(k), TyVar(_, name))) =>
    let new_name = StringUtil.insert(k, s, name);
    switch (TyTextShape.of_string(new_name)) {
    | None => Failed
    | Some(shape) =>
      let (tp, u_gen) = text_operand(shape, u_gen);
      Succeeded((CursorP(OnText(k + 1), tp), u_gen));
    };

  | (Construct(SChar(_)), _) => Failed
  | (Init, _) => failwith("Init action should not be performed")
  };
