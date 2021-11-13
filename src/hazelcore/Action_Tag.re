let mk_tag = (text: string, u_gen: MetaVarGen.t): (UHTag.t, MetaVarGen.t) =>
  UHTag.fix_holes(Tag(NotInTagHole, text), UHTag.Set.empty, u_gen);

let mk_text =
    (caret_index: int, text: string, u_gen: MetaVarGen.t)
    : (ZTag.t, MetaVarGen.t) => {
  let cursor = CursorPosition.OnText(caret_index);
  let (tag, u_gen) = mk_tag(text, u_gen);
  (ZTag.CursorTag(cursor, tag), u_gen);
};

let insert_text =
    (
      (caret_index: int, insert_text: string),
      text: string,
      u_gen: MetaVarGen.t,
    )
    : ActionOutcome.t((ZTag.t, MetaVarGen.t)) => {
  let text_len = String.length(insert_text);
  let new_text = text |> StringUtil.insert(caret_index, insert_text);
  Succeeded(mk_text(caret_index + text_len, new_text, u_gen));
};

let delete_text =
    (caret_index: int, text: string, u_gen: MetaVarGen.t)
    : ActionOutcome.t((ZTag.t, MetaVarGen.t)) =>
  caret_index == String.length(text)
    ? CursorEscaped(After)
    : Succeeded(
        mk_text(caret_index, text |> StringUtil.delete(caret_index), u_gen),
      );

let backspace_text =
    (caret_index: int, text: string, u_gen: MetaVarGen.t)
    : ActionOutcome.t((ZTag.t, MetaVarGen.t)) =>
  caret_index == 0
    ? CursorEscaped(Before)
    : {
      let new_text = text |> StringUtil.backspace(caret_index);
      Succeeded(mk_text(caret_index - 1, new_text, u_gen));
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
        SAnn | SLet | SLine | SLam | SList | SListNil | SInj | SCase |
        SParenthesized |
        SApPalette(_) |
        SCommentLine,
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
  | (_, CursorTag(OnText(_), EmptyTagHole(_))) => Failed

  /* Movement */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, ztag)

  /* Backspace and Delete */

  | (Backspace, CursorTag(OnDelim(_0, After), EmptyTagHole(_) as operand)) =>
    ZTag.is_after(ztag)
      ? Succeeded((ZTag.place_before(operand), u_gen))
      : CursorEscaped(Before)

  | (Delete, CursorTag(OnDelim(_0, Before), EmptyTagHole(_) as operand)) =>
    ZTag.is_before(ztag)
      ? Succeeded((ZTag.place_after(operand), u_gen)) : CursorEscaped(After)

  | (Backspace, CursorTag(OnDelim(_0, side), EmptyTagHole(_)))
  | (Delete, CursorTag(OnDelim(_0, side), EmptyTagHole(_))) =>
    CursorEscaped(side)

  | (Backspace, CursorTag(OnText(j), Tag(_, t))) =>
    switch (String.length(t)) {
    | 1 =>
      let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
      Succeeded((ZTag.place_before(tag_hole), u_gen));
    | _ => backspace_text(j, t, u_gen)
    }

  | (Delete, CursorTag(OnText(j), Tag(_, t))) =>
    switch (j, String.length(t)) {
    | (0, 1) =>
      let (tag_hole, u_gen) = UHTag.new_TagHole(u_gen);
      Succeeded((ZTag.place_before(tag_hole), u_gen));
    | (_, _) => delete_text(j, t, u_gen)
    }

  /* Construction */

  | (Construct(SOp(SSpace)), CursorTag(OnText(j), Tag(_) as tag)) =>
    switch (j) {
    | 0 => Succeeded((ZTag.place_after(tag), u_gen))
    | _ => ZTag.is_after(ztag) ? CursorEscaped(After) : Failed
    }

  | (Construct(SOp(_)), CursorTag(OnText(_), Tag(_, _))) => Failed

  | (Construct(SChar(c)), CursorTag(OnText(j), Tag(_, t)))
      when UHTag.is_tag_char(c.[0]) =>
    insert_text((j, c), t, u_gen)

  | (Construct(SChar(_)), CursorTag(OnText(_), Tag(_))) => Failed

  | (
      Construct(SOp(SSpace)),
      CursorTag(OnDelim(_0, After), EmptyTagHole(_)),
    ) =>
    CursorEscaped(After)

  | (Construct(SOp(_)), CursorTag(OnDelim(_, _), EmptyTagHole(_))) =>
    Failed

  | (Construct(SChar(c)), CursorTag(OnDelim(_, _), EmptyTagHole(_)))
      when UHTag.is_tag_char(c.[0]) =>
    insert_text((0, c), "", u_gen)

  | (Construct(SChar(_)), CursorTag(OnDelim(_, _), EmptyTagHole(_))) =>
    Failed

  | (Init, _) => failwith("Init action should not be performed.")
  };
