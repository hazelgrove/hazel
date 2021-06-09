let get =
    (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t, is_mac: bool)
    : option(ModelAction.t) =>
  switch (kc, cursor_info, is_mac) {
  | (Escape, _, _) => None
  | (Backspace, _, _) => Some(EditAction(Backspace))
  | (Delete, _, _) => Some(EditAction(Delete))
  | (ShiftTab, _, _) => Some(EditAction(MoveToPrevHole))
  | (Tab, _, _) => Some(EditAction(MoveToNextHole))
  | (GT, {CursorInfo.typed: OnType, _}, _) =>
    Some(EditAction(Construct(SOp(SArrow))))
  | (GT, _, _) => Some(EditAction(Construct(SOp(SGreaterThan))))
  | (Ampersand, _, _) => Some(EditAction(Construct(SOp(SAnd))))
  | (VBar, _, _) => Some(EditAction(Construct(SOp(SOr))))
  | (LeftParen, _, _) => Some(EditAction(Construct(SParenthesized)))
  | (Colon, _, _) => Some(EditAction(Construct(SAnn)))
  | (Equals, _, _) => Some(EditAction(Construct(SOp(SEquals))))
  | (Enter, _, _) => Some(EditAction(Construct(SLine)))
  | (Shift_Enter, _, _) => Some(EditAction(Construct(SCommentLine)))
  | (Backslash, _, _) => Some(EditAction(Construct(SLam)))
  | (Plus, _, _) => Some(EditAction(Construct(SOp(SPlus))))
  | (Minus, _, _) => Some(EditAction(Construct(SOp(SMinus))))
  | (Asterisk, _, _) => Some(EditAction(Construct(SOp(STimes))))
  | (Slash, _, _) => Some(EditAction(Construct(SOp(SDivide))))
  | (LT, _, _) => Some(EditAction(Construct(SOp(SLessThan))))
  | (Space, {CursorInfo.cursor_term: Line(_, CommentLine(_)), _}, _) =>
    Some(EditAction(Construct(SChar(" "))))
  | (Space, _, _) => Some(EditAction(Construct(SOp(SSpace))))
  | (Comma, _, _) => Some(EditAction(Construct(SOp(SComma))))
  | (LeftBracket, {CursorInfo.typed: OnType, _}, _) =>
    Some(EditAction(Construct(SList)))
  | (LeftBracket, _, _) => Some(EditAction(Construct(SListNil)))
  | (Semicolon, _, _) => Some(EditAction(Construct(SOp(SCons))))
  | (Alt_L, _, _) => Some(EditAction(Construct(SInj(L))))
  | (Alt_R, _, _) => Some(EditAction(Construct(SInj(R))))
  | (Alt_C, _, _) => Some(EditAction(Construct(SCase)))
  | (Pound, _, _) => Some(EditAction(Construct(SCommentLine)))
  | (Ctrl_Z, _, true) => None
  | (Ctrl_Z, _, false) => Some(Undo)
  | (Ctrl_Shift_Z, _, true) => None
  | (Ctrl_Shift_Z, _, false) => Some(Redo)
  | (Ctrl_Alt_I, _, _) => Some(EditAction(SwapUp))
  | (Ctrl_Alt_K, _, _) => Some(EditAction(SwapDown))
  | (Ctrl_Alt_J, _, _) => Some(EditAction(SwapLeft))
  | (Ctrl_Alt_L, _, _) => Some(EditAction(SwapRight))
  | (Meta_Z, _, true) => Some(Undo)
  | (Meta_Z, _, false) => None
  | (Meta_Shift_Z, _, true) => Some(Redo)
  | (Meta_Shift_Z, _, false) => None
  };
