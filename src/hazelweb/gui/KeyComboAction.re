let get = (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): Action.t => {
  let action_of_ci: CursorInfo.t => Action.t =
    switch (kc) {
    | Escape => raise(Not_found)
    | Backspace => (_ => Backspace)
    | Delete => (_ => Delete)
    | ShiftTab => (_ => MoveToPrevHole)
    | Tab => (_ => MoveToNextHole)
    | GT => (
        fun
        | {CursorInfo.typed: OnType, _} => Construct(SOp(SArrow))
        | _ => Construct(SOp(SGreaterThan))
      )
    | Ampersand => (_ => Construct(SOp(SAnd)))
    | VBar => (_ => Construct(SOp(SOr)))
    | LeftParen => (_ => Construct(SParenthesized))
    | Colon => (_ => Construct(SAnn))
    | Equals => (_ => Construct(SOp(SEquals)))
    | Enter => (_ => Construct(SLine))
    | Shift_Enter => (_ => Construct(SCommentLine))
    | Backslash => (_ => Construct(SLam))
    | Plus => (_ => Construct(SOp(SPlus)))
    | Minus => (_ => Construct(SOp(SMinus)))
    | Asterisk => (_ => Construct(SOp(STimes)))
    | Slash => (_ => Construct(SOp(SDivide)))
    | LT => (_ => Construct(SOp(SLessThan)))
    | Space => (
        fun
        | {CursorInfo.cursor_term: Line(_, CommentLine(_)), _} =>
          Construct(SChar(" "))
        | _ => Construct(SOp(SSpace))
      )
    | Comma => (_ => Construct(SOp(SComma)))
    | LeftBracket => (
        fun
        | {CursorInfo.typed: OnType, _} => Construct(SList)
        | _ => Construct(SListNil)
      )
    | Semicolon => (_ => Construct(SOp(SCons)))
    | Alt_L => (_ => Construct(SInj(L)))
    | Alt_R => (_ => Construct(SInj(R)))
    | Alt_C => (_ => Construct(SCase))
    | Pound => (_ => Construct(SCommentLine))
    | Ctrl_Z => raise(Not_found)
    | Ctrl_Shift_Z => raise(Not_found)
    | Ctrl_Alt_I => (_ => SwapUp)
    | Ctrl_Alt_K => (_ => SwapDown)
    | Ctrl_Alt_J => (_ => SwapLeft)
    | Ctrl_Alt_L => (_ => SwapRight)
    | Meta_Z => raise(Not_found)
    | Meta_Shift_Z => raise(Not_found)
    };
  action_of_ci(cursor_info);
};
