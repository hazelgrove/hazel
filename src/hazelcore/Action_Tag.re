let mk_text = (caret_index: int, text: string): ActionOutcome.t('success) =>
  if (UHTag.is_tag_name(text)) {
    Succeeded(
      ZTag.CursorTag(CursorPosition.OnText(caret_index), UHTag.Tag(text)),
    );
  } else {
    Failed;
  };

let insert_text =
    ((caret_index: int, insert_text: string), text: string)
    : ActionOutcome.t('success) =>
  mk_text(
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
  );

let delete_text = (caret_index: int, text: string): ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_text(caret_index, new_text);
  };

let backspace_text =
    (caret_index: int, text: string): ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_text(caret_index - 1, new_text);
  };

let move =
    (u_gen: MetaVarGen.t, a: Action.t, ztag: ZTag.t)
    : ActionOutcome.t((ZTag.t, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Tag.follow(path, ZTag.erase(ztag))) {
    | None => Failed
    | Some(ztag) => Succeeded((ztag, u_gen))
    }

  | MoveToPrevHole => CursorEscaped(Before)
  | MoveToNextHole => CursorEscaped(After)
  | MoveLeft =>
    switch (ZTag.move_cursor_left(ztag)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, u_gen))
    }
  | MoveRight =>
    switch (ZTag.move_cursor_right(ztag)) {
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

let perform =
    (u_gen: MetaVarGen.t, a: Action.t, ztag: ZTag.t)
    : ActionOutcome.t((ZTag.t, MetaVarGen.t)) =>
  switch (a, ztag) {
  /* Invalid actions */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SList | SListNil | SSum | SInj | SCase |
        SParenthesized |
        SOp(_) |
        SApPalette(_) |
        SCommentLine |
        SSumOp(_),
      ) |
      SwapUp |
      SwapDown |
      SwapLeft |
      SwapRight,
      _,
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorTag(cursor, tag)) when !ZTag.is_valid_cursor(cursor, tag) =>
    Failed
  | (_, CursorTag(OnOp(_), _))
  | (_, CursorTag(OnDelim(_), Tag(_)))
  | (_, CursorTag(OnText(_), TagHole(_))) => Failed

  /* Movement */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, ztag)

  /* Backspace and Delete */

  | (Backspace, CursorTag(OnDelim(_0, After), TagHole(_) as operand)) =>
    let ztag = ZTag.place_before(operand);
    ZTag.is_after(ztag) ? Succeeded((ztag, u_gen)) : CursorEscaped(Before);
  | (Delete, CursorTag(OnDelim(_0, Before), TagHole(_) as operand)) =>
    let ztag = operand |> ZTag.place_after;
    ZTag.is_before(ztag) ? Succeeded((ztag, u_gen)) : CursorEscaped(After);

  | (Backspace, CursorTag(OnDelim(_0, Before), TagHole(_))) =>
    CursorEscaped(Before)
  | (Delete, CursorTag(OnDelim(_0, After), TagHole(_))) =>
    CursorEscaped(After)

  | (Delete, CursorTag(OnText(j), Tag(t))) =>
    switch (delete_text(j, t)) {
    | Succeeded(ztag) => Succeeded((ztag, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }
  | (Backspace, CursorTag(OnText(j), Tag(t))) =>
    switch (backspace_text(j, t)) {
    | Succeeded(ztag) => Succeeded((ztag, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }

  /* Construction */
  | (Construct(SChar(s)), CursorTag(_, TagHole(_)))
      when UHTag.is_tag_name(s) =>
    switch (insert_text((0, s), "")) {
    | Succeeded(ztag) => Succeeded((ztag, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }

  | (Construct(SChar(s)), CursorTag(OnText(0), Tag(t)))
      when UHTag.is_tag_name(s) =>
    switch (insert_text((0, s), t)) {
    | Succeeded(ztag) => Succeeded((ztag, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }
  | (Construct(SChar(s)), CursorTag(OnText(j), Tag(t)))
      when
        s |> String.to_seq |> List.of_seq |> List.for_all(UHTag.is_tag_char) =>
    switch (insert_text((j, s), t)) {
    | Succeeded(ztag) => Succeeded((ztag, u_gen))
    | (CursorEscaped(_) | Failed) as outcome => outcome
    }

  | (Construct(SChar(_)), CursorTag(_, _)) => Failed

  | (Init, _) => failwith("Init action should not be performed.")
  };
