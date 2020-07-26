let move =
    (a: Action_common.t, ztp: ZTPat.t, u_gen: MetaVarGen.t)
    : ActionOutcome.t((ZTPat.t, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_TPat.follow(path, ztp |> ZTPat.erase)) {
    | None => Failed
    | Some(ztp) => Succeeded((ztp, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_TPat.holes_z(ztp, [])))
    ) {
    | None => Failed
    | Some(_) => Failed
    /*switch (CursorPath_TPat.of_steps(steps, ztp |> ZTPat.erase)) {
      | None => Failed
      | Some(path) => move(MoveTo(path), ztp, u_gen)
      }*/
    }
  | MoveToNextHole => Failed
  | MoveLeft =>
    ztp
    |> ZTPat.move_cursor_left
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(Before), ztp =>
         Succeeded((ztp, u_gen))
       )
  | MoveRight =>
    ztp
    |> ZTPat.move_cursor_right
    |> OptUtil.map_default(~default=ActionOutcome.CursorEscaped(After), ztp =>
         Succeeded((ztp, u_gen))
       )
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
      ++ Sexplib.Sexp.to_string(Action_common.sexp_of_t(a)),
    )
  };

let rec perform_operand =
        (a: Action_common.t, zoperand: ZTPat.zoperand, u_gen: MetaVarGen.t)
        : ActionOutcome.t((ZTPat.t, MetaVarGen.t)) =>
  switch (a, zoperand) {
  | (
      UpdateApPalette(_) |
      Construct(
        SAsc | SLet | SLine | SLam | SListNil | SInj(_) | SCase | SDefine |
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
  | (_, CursorP(cursor, operand))
      when !ZTPat.is_valid_cursor(cursor, operand) =>
    Failed

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(a, zoperand, u_gen)

  | (Backspace, CursorP(OnDelim(_, Before), _)) => CursorEscaped(Before)

  | (Delete, CursorP(OnDelim(_, After), _)) => CursorEscaped(After)

  | (Backspace, CursorP(OnDelim(k, After), EmptyHole(u))) =>
    Succeeded((CursorP(OnDelim(k, Before), EmptyHole(u)), u_gen))

  | (Backspace, CursorP(OnDelim(_, After), _)) => Failed

  | (Delete, CursorP(OnDelim(k, Before), operand)) =>
    perform_operand(Backspace, CursorP(OnDelim(k, After), operand), u_gen)

  /* Backspace and delete on tyvar */
  | (Backspace, CursorP(OnText(k), TyVar(err, x))) =>
    ZTPat.is_before(zoperand)
      ? CursorEscaped(Before)
      : {
        let new_x = StringUtil.backspace(k, x);
        if (StringUtil.is_empty(new_x)) {
          let (new_hole, u_gen) = ZTPat.new_EmptyHole(u_gen);
          Succeeded((new_hole, u_gen));
        } else {
          Succeeded((CursorP(OnText(k - 1), TyVar(err, new_x)), u_gen));
        };
      }

  | (Backspace, CursorP(OnText(_), _)) => Failed

  | (Delete, CursorP(OnText(k), TyVar(err, x))) =>
    ZTPat.is_after(zoperand)
      ? CursorEscaped(After)
      : {
        let new_x = StringUtil.delete(k, x);
        if (StringUtil.is_empty(new_x)) {
          let (new_hole, u_gen) = ZTPat.new_EmptyHole(u_gen);
          Succeeded((new_hole, u_gen));
        } else {
          Succeeded((CursorP(OnText(k - 1), TyVar(err, new_x)), u_gen));
        };
      }

  | (Delete, CursorP(OnText(_), _)) => Failed
  /* Construction */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
    perform_operand(MoveRight, zoperand, u_gen)

  | (Construct(SOp(_)), _) => Failed

  | (Construct(SChar(s)), CursorP(_, EmptyHole(_))) =>
    Succeeded((CursorP(OnText(1), TyVar(NotInVarHole, s)), u_gen))

  | (Construct(SChar(s)), CursorP(OnText(k), TyVar(err, x))) =>
    let new_x = StringUtil.insert(k, s, x);
    Succeeded((CursorP(OnText(k + 1), TyVar(err, new_x)), u_gen));

  | (Construct(SChar(_)), _) => Failed
  | (Init, _) => failwith("Init action should not be performed")
  };

let perform =
    (a: Action_common.t, ztp: ZTPat.t, u_gen: MetaVarGen.t)
    : ActionOutcome.t((ZTPat.t, MetaVarGen.t)) =>
  perform_operand(a, ztp, u_gen);
