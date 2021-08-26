let table: Hashtbl.t(HazelKeyCombos.t, CursorInfo.t => Action.t) =
  [
    (HazelKeyCombos.Backspace, _ => Action.Backspace),
    (Delete, _ => Delete),
    (ShiftTab, _ => MoveToPrevHole),
    (Tab, _ => MoveToNextHole),
    (
      GT,
      fun
      | {CursorInfo.typed: OnType, _} => Construct(SOp(SArrow))
      | _ => Construct(SOp(SGreaterThan)),
    ),
    (Ampersand, _ => Construct(SOp(SAnd))),
    (
      VBar,
      fun
      | {CursorInfo.typed: OnType, _} => Construct(SOp(SVBar))
      | _ => Construct(SOp(SOr)),
    ),
    (LeftParen, _ => Construct(SParenthesized)),
    (Colon, _ => Construct(SAnn)),
    (Equals, _ => Construct(SOp(SEquals))),
    (Enter, _ => Construct(SLine)),
    (Backslash, _ => Construct(SLam)),
    (Plus, _ => Construct(SOp(SPlus))),
    (Minus, _ => Construct(SOp(SMinus))),
    (Asterisk, _ => Construct(SOp(STimes))),
    (Slash, _ => Construct(SOp(SDivide))),
    (LT, _ => Construct(SOp(SLessThan))),
    (
      Space,
      fun
      | {CursorInfo.cursor_term: Line(_, CommentLine(_)), _} =>
        Construct(SChar(" "))
      | _ => Construct(SOp(SSpace)),
    ),
    (Comma, _ => Construct(SOp(SComma))),
    (
      LeftBracket,
      fun
      | {CursorInfo.typed: OnType, _} => Construct(SList)
      | _ => Construct(SListNil),
    ),
    (Semicolon, _ => Construct(SOp(SCons))),
    (Alt_I, _ => Construct(SInj)),
    (Alt_C, _ => Construct(SCase)),
    (Pound, _ => Construct(SCommentLine)),
    (Shift_Enter, _ => Construct(SCommentLine)),
    (Alt_Up, _ => SwapUp),
    (Alt_Down, _ => SwapDown),
    (Alt_Left, _ => SwapLeft),
    (Alt_Right, _ => SwapRight),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

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
  | Alt_I => construct(SInj)
  | Alt_C => construct(SCase)
  | Pound => construct(SCommentLine)
  | Ctrl_S => Some(SerializeToConsole)
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
