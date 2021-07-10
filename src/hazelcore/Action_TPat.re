module TyIdUtil = {
  let insert = (k, s, x) =>
    StringUtil.insert(k, s, TyId.to_string(x)) |> TyId.of_string;
  let delete = (k, x) =>
    StringUtil.delete(k, TyId.to_string(x)) |> TyId.of_string;
  let backspace = (k, x) =>
    StringUtil.backspace(k, TyId.to_string(x)) |> TyId.of_string;
  let is_empty = x => TyId.to_string(x) |> StringUtil.is_empty;
};

let rec move = (a: Action.t, zp: ZTPat.t): ActionOutcome.t(ZTPat.t) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_TPat.follow(path, ZTPat.erase(zp))) {
    | None => Failed
    | Some(zp) => Succeeded(zp)
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_TPat.holes_z(zp, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_TPat.of_steps(steps, ZTPat.erase(zp))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), zp)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_TPat.holes_z(zp, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_TPat.of_steps(steps, ZTPat.erase(zp))) {
      | None => Failed
      | Some(path) => move(MoveTo(path), zp)
      }
    }
  | MoveLeft =>
    switch (ZTPat.move_cursor_left(zp)) {
    | None => Failed
    | Some(zp) => Succeeded(zp)
    }
  | MoveRight =>
    switch (ZTPat.move_cursor_right(zp)) {
    | None => Failed
    | Some(zp) => Succeeded(zp)
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

let rec perform = (a: Action.t, zp: ZTPat.t): ActionOutcome.t(ZTPat.t) =>
  switch (a, zp) {
  | (
      UpdateApPalette(_) |
      Construct(
        SCommentLine | SAnn | SLet | SLine | SLam | SListNil | SInj(_) | SCase |
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
  | (_, CursorP(cursor, zp)) when !ZTPat.is_valid_cursor(cursor, zp) =>
    Failed

  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(a, zp)

  | (Backspace, CursorP(OnDelim(_, Before), _)) => CursorEscaped(Before)

  | (Delete, CursorP(OnDelim(_, After), _)) => CursorEscaped(After)

  | (Backspace, CursorP(OnDelim(k, After), EmptyHole)) =>
    Succeeded(CursorP(OnDelim(k, Before), EmptyHole))

  | (Backspace, CursorP(OnDelim(_, After), _)) => Failed

  | (Delete, CursorP(OnDelim(k, Before), zp)) =>
    perform(Backspace, CursorP(OnDelim(k, After), zp))

  /* Backspace and delete on tyvar */
  | (Backspace, CursorP(OnText(k), TyVar(_, x))) =>
    ZTPat.is_before(zp)
      ? CursorEscaped(Before)
      : {
        let new_x = TyIdUtil.backspace(k, x);
        if (TyIdUtil.is_empty(new_x)) {
          Succeeded(ZTPat.place_after(EmptyHole));
        } else {
          Succeeded(CursorP(OnText(k - 1), TPat.tyvar_of_tyid(new_x)));
        };
      }

  | (Backspace, CursorP(OnText(_), _)) => Failed

  | (Delete, CursorP(OnText(k), TyVar(_, x))) =>
    ZTPat.is_after(zp)
      ? CursorEscaped(After)
      : {
        let new_x = TyIdUtil.delete(k, x);
        if (TyIdUtil.is_empty(new_x)) {
          Succeeded(ZTPat.place_before(EmptyHole));
        } else {
          Succeeded(CursorP(OnText(k - 1), TPat.tyvar_of_tyid(new_x)));
        };
      }

  | (Delete, CursorP(OnText(_), _)) => Failed
  /* Construction */
  | (Construct(SOp(SSpace)), CursorP(OnDelim(_, After), _)) =>
    perform(MoveRight, zp)

  | (Construct(SOp(_)), _) => Failed

  | (Construct(SChar(s)), CursorP(_, EmptyHole)) =>
    Succeeded(CursorP(OnText(1), TPat.tyvar_of_tyid(TyId.of_string(s))))

  | (Construct(SChar(s)), CursorP(OnText(k), TyVar(_, x))) =>
    let new_x = TyIdUtil.insert(k, s, x);
    Succeeded(CursorP(OnText(k + 1), TPat.tyvar_of_tyid(new_x)));

  | (Construct(SChar(_)), _) => Failed
  | (Init, _) => failwith("Init action should not be performed")
  };
