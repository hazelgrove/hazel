let table: Hashtbl.t(HazelKeyCombos.t, CursorInfo_common.t => Action_common.t) =
  [
    (HazelKeyCombos.Backspace, _ => Action_common.Backspace),
    (Delete, _ => Action_common.Delete),
    (ShiftTab, _ => Action_common.MoveToPrevHole),
    (Tab, _ => Action_common.MoveToNextHole),
    (
      GT,
      fun
      | {CursorInfo_common.typed: OnType, _} =>
        Action_common.Construct(SOp(SArrow))
      | _ => Action_common.Construct(SOp(SGreaterThan)),
    ),
    (Ampersand, _ => Action_common.Construct(SOp(SAnd))),
    (VBar, _ => Action_common.Construct(SOp(SOr))),
    (LeftParen, _ => Action_common.Construct(SParenthesized)),
    (Colon, _ => Action_common.Construct(SAsc)),
    (Equals, _ => Action_common.Construct(SOp(SEquals))),
    (Enter, _ => Action_common.Construct(SLine)),
    (Backslash, _ => Action_common.Construct(SLam)),
    (Plus, _ => Action_common.Construct(SOp(SPlus))),
    (Minus, _ => Action_common.Construct(SOp(SMinus))),
    (Asterisk, _ => Action_common.Construct(SOp(STimes))),
    (Slash, _ => Action_common.Construct(SOp(SDivide))),
    (LT, _ => Action_common.Construct(SOp(SLessThan))),
    (
      Space,
      fun
      | {CursorInfo_common.cursor_term: Line(_, CommentLine(_)), _} =>
        Action_common.Construct(SChar(" "))
      | _ => Action_common.Construct(SOp(SSpace)),
    ),
    (Comma, _ => Action_common.Construct(SOp(SComma))),
    (LeftBracket, _ => Action_common.Construct(SLeftBracket)),
    (LeftQuotation, _ => Action_common.Construct(SQuote)),
    (Dollar, _ => Action_common.Construct(SChar("$"))),
    (
      LeftBracket,
      fun
      | {CursorInfo_common.typed: OnType, _} =>
        Action_common.Construct(SList)
      | _ => Action_common.Construct(SListNil),
    ),
    (Semicolon, _ => Action_common.Construct(SOp(SCons))),
    (Alt_L, _ => Action_common.Construct(SInj(L))),
    (Alt_R, _ => Action_common.Construct(SInj(R))),
    (Alt_C, _ => Action_common.Construct(SCase)),
    (Pound, _ => Action_common.Construct(SCommentLine)),
    (Shift_Enter, _ => Action_common.Construct(SCommentLine)),
    (Ctrl_Alt_I, _ => Action_common.SwapUp),
    (Ctrl_Alt_K, _ => Action_common.SwapDown),
    (Ctrl_Alt_J, _ => Action_common.SwapLeft),
    (Ctrl_Alt_L, _ => Action_common.SwapRight),
  ]
  |> List.to_seq
  |> Hashtbl.of_seq;

let get =
    (cursor_info: CursorInfo_common.t, kc: HazelKeyCombos.t): Action_common.t => {
  let action_of_ci = Hashtbl.find(table, kc);
  action_of_ci(cursor_info);
};
