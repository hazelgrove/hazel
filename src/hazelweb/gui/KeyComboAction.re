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
    (VBar, _ => Construct(SOp(SOr))),
    (LeftParen, _ => Construct(SParenthesized)),
    (Colon, _ => Construct(SAsc)),
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
    (Ctrl_Pound, _ => Action.RenumberHoles),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let get = (cursor_info: CursorInfo.t, kc: HazelKeyCombos.t): Action.t => {
  let action_of_ci = Hashtbl.find(table, kc);
  action_of_ci(cursor_info);
};
