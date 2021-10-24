let get_model_action =
    (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): option(ModelAction.t) => {
  let construct = (shape: Action.shape): option(ModelAction.t) =>
    Some(EditAction(Construct(shape)));

  let (cursor_on_type, cursor_on_comment, cursor_on_stringlit) =
    switch (cursor_info) {
    | {typed: OnType, _} => (true, false, false)
    | {cursor_term: Line(_, CommentLine(_)), _} => (false, true, false)
    | {
        cursor_term:
          Exp(OnText(_), StringLit(_)) | Pat(OnText(_), StringLit(_)),
        _,
      } => (
        false,
        false,
        true,
      )
    | _ => (false, false, false)
    };

  switch (kc) {
  | Escape => None
  | Backspace => Some(EditAction(Backspace))
  | Delete => Some(EditAction(Delete))
  | ShiftTab => Some(EditAction(MoveToPrevHole))
  | Tab => Some(EditAction(MoveToNextHole))
  | GT when cursor_on_type => construct(SOp(SArrow))
  | GT when cursor_on_stringlit => construct(SChar(">"))
  | GT => construct(SOp(SGreaterThan))
  | Ampersand when cursor_on_stringlit => construct(SChar("&"))
  | Ampersand => construct(SOp(SAnd))
  | VBar when cursor_on_type => construct(SOp(SVBar))
  | VBar when cursor_on_stringlit => construct(SChar("|"))
  | VBar => construct(SOp(SOr))
  | LeftParen when cursor_on_stringlit => construct(SChar("("))
  | LeftParen => construct(SParenthesized)
  | Colon when cursor_on_stringlit => construct(SChar(":"))
  | Colon => construct(SAnn)
  | Equals when cursor_on_stringlit => construct(SChar("="))
  | Equals => construct(SOp(SEquals))
  // TODO: Enter when cursor_on_stringlit
  | Enter => construct(SLine)
  | Shift_Enter => construct(SCommentLine)
  | Backslash when cursor_on_stringlit => construct(SChar("\\"))
  | Backslash => construct(SLam)
  | Plus when cursor_on_stringlit => construct(SChar("+"))
  | Plus => construct(SOp(SPlus))
  | Minus when cursor_on_stringlit => construct(SChar("-"))
  | Minus => construct(SOp(SMinus))
  | Asterisk when cursor_on_stringlit => construct(SChar("*"))
  | Asterisk => construct(SOp(STimes))
  | Slash when cursor_on_stringlit => construct(SChar("/"))
  | Slash => construct(SOp(SDivide))
  | LT when cursor_on_stringlit => construct(SChar("<"))
  | LT => construct(SOp(SLessThan))
  | Space when cursor_on_comment => construct(SChar(" "))
  | Space when cursor_on_stringlit => construct(SChar(" "))
  | Space => construct(SOp(SSpace))
  | Comma when cursor_on_stringlit => construct(SChar(","))
  | Comma => construct(SOp(SComma))
  | LeftBracket when cursor_on_type => construct(SList)
  | LeftBracket when cursor_on_stringlit => construct(SChar("["))
  | LeftBracket => construct(SListNil)
  | Semicolon when cursor_on_stringlit => construct(SChar(";"))
  | Semicolon => construct(SOp(SCons))
  | Quote when cursor_on_stringlit => construct(SChar("\""))
  | Quote => construct(SQuote)
  | Alt_L => construct(SInj(L))
  | Alt_R => construct(SInj(R))
  | Alt_C => construct(SCase)
  | Pound when cursor_on_stringlit => construct(SChar("#"))
  | Pound => construct(SCommentLine)
  | Ctrl_S => Some(SerializeToConsole(UHExp))
  | CtrlOrCmd_Z => Some(Undo)
  | CtrlOrCmd_Shift_Z => Some(Redo)
  | Up => Some(MoveAction(Key(ArrowUp)))
  | Down => Some(MoveAction(Key(ArrowDown)))
  | Left => Some(MoveAction(Key(ArrowLeft)))
  | Right => Some(MoveAction(Key(ArrowRight)))
  | Home => Some(MoveAction(Key(Home)))
  | End => Some(MoveAction(Key(End)))
  | Alt_Up => Some(EditAction(SwapUp))
  | Alt_Down => Some(EditAction(SwapDown))
  | Alt_Left => Some(EditAction(SwapLeft))
  | Alt_Right => Some(EditAction(SwapRight))
  };
};
