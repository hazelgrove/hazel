let get = (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): option(Action.t) =>
  switch (kc, cursor_info) {
  | (Escape, _) => None
  | (Backspace, _) => Some(Backspace)
  | (Delete, _) => Some(Delete)
  | (ShiftTab, _) => Some(MoveToPrevHole)
  | (Tab, _) => Some(MoveToNextHole)
  | (GT, {CursorInfo.typed: OnType, _}) => Some(Construct(SOp(SArrow)))
  | (GT, _) => Some(Construct(SOp(SGreaterThan)))
  | (Ampersand, _) => Some(Construct(SOp(SAnd)))
  | (VBar, _) => Some(Construct(SOp(SOr)))
  | (LeftParen, _) => Some(Construct(SParenthesized))
  | (Colon, _) => Some(Construct(SAnn))
  | (Equals, _) => Some(Construct(SOp(SEquals)))
  | (Enter, _) => Some(Construct(SLine))
  | (Shift_Enter, _) => Some(Construct(SCommentLine))
  | (Backslash, _) => Some(Construct(SLam))
  | (Plus, _) => Some(Construct(SOp(SPlus)))
  | (Minus, _) => Some(Construct(SOp(SMinus)))
  | (Asterisk, _) => Some(Construct(SOp(STimes)))
  | (Slash, _) => Some(Construct(SOp(SDivide)))
  | (LT, _) => Some(Construct(SOp(SLessThan)))
  | (Space, {CursorInfo.cursor_term: Line(_, CommentLine(_)), _}) =>
    Some(Construct(SChar(" ")))
  | (Space, _) => Some(Construct(SOp(SSpace)))
  | (Comma, _) => Some(Construct(SOp(SComma)))
  | (LeftBracket, {CursorInfo.typed: OnType, _}) => Some(Construct(SList))
  | (LeftBracket, _) => Some(Construct(SListNil))
  | (Semicolon, _) => Some(Construct(SOp(SCons)))
  | (Alt_L, _) => Some(Construct(SInj(L)))
  | (Alt_R, _) => Some(Construct(SInj(R)))
  | (Alt_C, _) => Some(Construct(SCase))
  | (Pound, _) => Some(Construct(SCommentLine))
  | (Ctrl_Z, _) => None
  | (Ctrl_Shift_Z, _) => None
  | (Ctrl_Alt_I, _) => Some(SwapUp)
  | (Ctrl_Alt_K, _) => Some(SwapDown)
  | (Ctrl_Alt_J, _) => Some(SwapLeft)
  | (Ctrl_Alt_L, _) => Some(SwapRight)
  | (Meta_Z, _) => None
  | (Meta_Shift_Z, _) => None
  };
