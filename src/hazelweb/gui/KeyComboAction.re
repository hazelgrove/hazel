let get_model_action =
    (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): option(ModelAction.t) => {
  let construct = (shape: Action.shape): option(ModelAction.t) =>
    Some(EditAction(Construct(shape)));

  let (cursor_on_type, cursor_on_comment) =
    switch (cursor_info) {
    | {typed: OnType, _} => (true, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true)
    | _ => (false, false)
    };

  switch (kc) {
  | Escape => None
  | Backspace => Some(EditAction(Backspace))
  | Delete => Some(EditAction(Delete))
  | ShiftTab => Some(EditAction(MoveToPrevHole))
  | Tab => Some(EditAction(MoveToNextHole))
  | GT when cursor_on_type => construct(SOp(SArrow))
  | GT => construct(SOp(SGreaterThan))
  | Ampersand => construct(SOp(SAnd))
  | VBar when cursor_on_type => construct(SOp(SVBar))
  | VBar => construct(SOp(SOr))
  | LeftParen => construct(SParenthesized)
  | Colon => construct(SAnn)
  | Equals => construct(SOp(SEquals))
  | Enter => construct(SLine)
  | Shift_Enter => construct(SCommentLine)
  | Backslash => construct(SLam)
  | Plus => construct(SOp(SPlus))
  | Minus => construct(SOp(SMinus))
  | Asterisk => construct(SOp(STimes))
  | Slash => construct(SOp(SDivide))
  | LT => construct(SOp(SLessThan))
  | Space when cursor_on_comment => construct(SChar(" "))
  | Space => construct(SOp(SSpace))
  | Comma => construct(SOp(SComma))
  | LeftBracket when cursor_on_type => construct(SList)
  | LeftBracket => construct(SListNil)
  | Semicolon => construct(SOp(SCons))
  | Alt_L => construct(SInj(L))
  | Alt_R => construct(SInj(R))
  | Alt_C => construct(SCase)
  | Pound => construct(SCommentLine)
  | Ctrl_S => Some(SerializeToConsole)
  | Ctrl_Z => Some(Undo)
  | Ctrl_Shift_Z => Some(Redo)
  | Ctrl_Alt_I => Some(EditAction(SwapUp))
  | Ctrl_Alt_K => Some(EditAction(SwapDown))
  | Ctrl_Alt_J => Some(EditAction(SwapLeft))
  | Ctrl_Alt_L => Some(EditAction(SwapRight))
  };
};
