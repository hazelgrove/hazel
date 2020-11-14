let table: Hashtbl.t(HazelKeyCombos.t, CursorInfo.t => Action.t) =
  [
    (HazelKeyCombos.Backspace, _ => Action.Backspace),
    (Delete, _ => Action.Delete),
    (ShiftTab, _ => Action.MoveToPrevHole),
    (Tab, _ => Action.MoveToNextHole),
    (
      GT,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SOp(SArrow))
      | _ => Action.Construct(SOp(SGreaterThan)),
    ),
    (Ampersand, _ => Action.Construct(SOp(SAnd))),
    (VBar, _ => Action.Construct(SOp(SOr))),
    (LeftParen, _ => Action.Construct(SParenthesized)),
    (Colon, _ => Action.Construct(SAsc)),
    (Equals, _ => Action.Construct(SOp(SEquals))),
    (Enter, _ => Action.Construct(SLine)),
    (Backslash, _ => Action.Construct(SLam)),
    (Plus, _ => Action.Construct(SOp(SPlus))),
    (Minus, _ => Action.Construct(SOp(SMinus))),
    (Asterisk, _ => Action.Construct(SOp(STimes))),
    (Slash, _ => Action.Construct(SOp(SDivide))),
    (LT, _ => Action.Construct(SOp(SLessThan))),
    (
      Space,
      fun
      | {CursorInfo.cursor_term: Line(_, CommentLine(_)), _} =>
        Action.Construct(SChar(" "))
      | _ => Action.Construct(SOp(SSpace)),
    ),
    (Comma, _ => Action.Construct(SOp(SComma))),
    (
      LeftBracket,
      fun
      | {CursorInfo.typed: OnType, _} => Action.Construct(SList)
      | _ => Action.Construct(SListNil),
    ),
    (Semicolon, _ => Action.Construct(SOp(SCons))),
    (Alt_L, _ => Action.Construct(SInj(L))),
    (Alt_R, _ => Action.Construct(SInj(R))),
    (Alt_C, _ => Action.Construct(SCase)),
    (Pound, _ => Action.Construct(SCommentLine)),
    (Shift_Enter, _ => Action.Construct(SCommentLine)),
    (Ctrl_Alt_I, _ => Action.SwapUp),
    (Ctrl_Alt_K, _ => Action.SwapDown),
    (Ctrl_Alt_J, _ => Action.SwapLeft),
    (Ctrl_Alt_L, _ => Action.SwapRight),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let get = (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): Action.t => {
  let action_of_ci = Hashtbl.find(table, kc);
  action_of_ci(cursor_info);
};
