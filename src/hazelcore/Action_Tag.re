let mk_text =
    (u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t('success) => {
  let text_cursor = CursorPosition.OnText(caret_index);
  switch (TextShape.of_text(text)) {
  | InvalidTextShape(_) =>
    if (text |> StringUtil.is_empty) {
      let (ztag, u_gen) = u_gen |> ZTag.new_TagHole;
      Succeeded((ztag, u_gen));
    } else {
      Failed;
    }
  | Tag(t) => Succeeded((CursorTag(text_cursor, UHTag.Tag(t)), u_gen))
  | ExpandingKeyword(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Underscore
  | Var(_) => Failed
  };
};

let insert_text =
    (
      u_gen: MetaVarGen.t,
      (caret_index: int, insert_text: string),
      text: string,
    )
    : ActionOutcome.t('success) =>
  mk_text(
    u_gen,
    caret_index + String.length(insert_text),
    text |> StringUtil.insert(caret_index, insert_text),
  );

let delete_text =
    (u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t('success) =>
  if (caret_index == String.length(text)) {
    CursorEscaped(After);
  } else {
    let new_text = text |> StringUtil.delete(caret_index);
    mk_text(u_gen, caret_index, new_text);
  };

let backspace_text =
    (u_gen: MetaVarGen.t, caret_index: int, text: string)
    : ActionOutcome.t('success) =>
  if (caret_index == 0) {
    CursorEscaped(Before);
  } else {
    let new_text = text |> StringUtil.backspace(caret_index);
    mk_text(u_gen, caret_index - 1, new_text);
  };

// TODO: redo this with escapes
let rec move =
        (u_gen: MetaVarGen.t, a: Action.t, ztag: ZTag.t)
        : ActionOutcome.t((ZTag.t, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Tag.follow(path, ztag |> ZTag.erase)) {
    | None => Failed
    | Some(ztag) => Succeeded((ztag, u_gen))
    }

  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Tag.holes_z(ztag, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Tag.of_steps(steps, ztag |> ZTag.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), ztag)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Tag.holes_z(ztag, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Tag.of_steps(steps, ztag |> ZTag.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), ztag)
      }
    }
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

let rec perform =
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
  | (_, CursorTag(OnOp(_), _)) => Failed
  | (_, CursorTag(cursor, tag)) when !ZTag.is_valid_cursor(cursor, tag) =>
    Failed

  /* Movement */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, ztag)

  /* Backspace and Delete */

  | (Backspace, CursorTag(_, TagHole(_) as operand)) =>
    let ztag = operand |> ZTag.place_before;
    ztag |> ZTag.is_after ? Succeeded((ztag, u_gen)) : CursorEscaped(Before);
  | (Delete, CursorTag(_, TagHole(_) as operand)) =>
    let ztag = operand |> ZTag.place_after;
    ztag |> ZTag.is_before ? Succeeded((ztag, u_gen)) : CursorEscaped(After);

  // TODO: redo this for text cursors
  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorTag(OnDelim(_, Before), _)) =>
    perform(u_gen, MoveLeft, ztag)
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorTag(OnDelim(_, After), _)) =>
    perform(u_gen, MoveRight, ztag)

  | (Backspace, CursorTag(OnDelim(_, After), Tag(_))) =>
    let (hole, u_gen) = UHTag.new_TagHole(u_gen);
    Succeeded((ZTag.place_after(hole), u_gen));

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorTag(OnDelim(k, Before), tag)) =>
    perform(u_gen, Backspace, CursorTag(OnDelim(k, After), tag))

  | (Delete, CursorTag(OnText(j), Tag(t))) => delete_text(u_gen, j, t)
  | (Backspace, CursorTag(OnText(j), Tag(t))) => backspace_text(u_gen, j, t)

  /* Construction */
  | (Construct(SChar(s)), CursorTag(_, TagHole(_)))
      when UHTag.is_tag_name(s) =>
    insert_text(u_gen, (0, s), "")

  | (Construct(SChar(s)), CursorTag(OnText(0), Tag(t)))
      when UHTag.is_tag_name(s) =>
    insert_text(u_gen, (0, s), t)
  | (Construct(SChar(s)), CursorTag(OnText(j), Tag(t)))
      when
        s |> String.to_seq |> List.of_seq |> List.for_all(UHTag.is_tag_char) =>
    insert_text(u_gen, (j, s), t)

  | (Construct(SChar(_)), CursorTag(_, _)) => Failed

  | (Init, _) => failwith("Init action should not be performed.")
  };
