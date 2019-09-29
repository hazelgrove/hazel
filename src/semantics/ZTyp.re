open GeneralUtil;
open SemanticsCommon;

[@deriving sexp]
type t = zopseq
and zopseq = ZOpSeq.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
and zoperand =
  | CursorT(cursor_position, UHTyp.operand)
  | ParenthesizedZ(t)
  | ListZ(t)
and zoperator = (side, UHTyp.operator);

let valid_cursors: UHTyp.operand => list(cursor_position) =
  fun
  | Hole
  | Unit
  | Num
  | Bool => delim_cursors(1)
  | Parenthesized(_)
  | List(_) => delim_cursors(2);

let is_valid_cursor = (cursor: cursor_position, operand: UHTyp.operand): bool =>
  valid_cursors(operand) |> contains(cursor);

let erase_operator =
  fun
  | (_, operator) => operator;

let rec erase = (zty: t): UHTyp.t => erase_opseq(zty)
and erase_opseq = opseq =>
  ZOpSeq.erase(~erase_operand, ~erase_operator, opseq)
and erase_operand =
  fun
  | CursorT(_, operand) => operand
  | ParenthesizedZ(zty) => Parenthesized(erase(zty))
  | ListZ(zty) => List(erase(zty));

let rec is_before = (zty: t): bool => is_before_opseq(zty)
and is_before_opseq = opseq => ZOpSeq.is_before(~is_before_operand, opseq)
and is_before_operand =
  fun
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Num)
  | CursorT(cursor, Bool)
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | ParenthesizedZ(_) => false
  | ListZ(_) => false;

let rec is_after = (zty: t): bool => is_after_opseq(zty)
and is_after_opseq = opseq => ZOpSeq.is_after(~is_after_operand, opseq)
and is_after_operand =
  fun
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Num)
  | CursorT(cursor, Bool) => cursor == OnDelim(0, After)
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | ParenthesizedZ(_) => false
  | ListZ(_) => false;

let rec place_before = (uty: UHTyp.t): t => place_before_opseq(uty)
and place_before_opseq = opseq =>
  ZOpSeq.place_before(~place_before_operand, opseq)
and place_before_operand =
  fun
  | (Hole | Unit | Num | Bool | Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(0, Before), operand);
let place_before_operator = op => (Before, op);

let rec place_after = (uty: UHTyp.t): t => place_after_opseq(uty)
and place_after_opseq = opseq =>
  ZOpSeq.place_after(~place_after_operand, opseq)
and place_after_operand =
  fun
  | (Hole | Unit | Num | Bool) as operand =>
    CursorT(OnDelim(0, After), operand)
  | (Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(1, After), operand);
let place_after_operator = op => (After, op);

let place_cursor =
    (cursor: cursor_position, operand: UHTyp.operand): option(zoperand) =>
  is_valid_cursor(cursor, operand) ? Some(CursorT(cursor, operand)) : None;

let move_cursor_left_operator =
  fun
  | (Before, _) => None
  | (After, op) => Some((Before, op));

let rec move_cursor_left = (zty: t): option(t) =>
  move_cursor_left_opseq(zty)
and move_cursor_left_opseq = opseq =>
  ZOpSeq.move_cursor_left(
    ~move_cursor_left_operand,
    ~move_cursor_left_operator,
    ~place_after_operand,
    ~place_after_operator,
    ~erase_operand,
    ~erase_operator,
    opseq,
  )
and move_cursor_left_operand =
  fun
  | zoperand when is_before_operand(zoperand) => None
  | CursorT(OnText(_) | Staging(_), _) => None
  | CursorT(OnDelim(k, After), ty) =>
    Some(CursorT(OnDelim(k, Before), ty))
  | CursorT(OnDelim(_, Before), Hole | Unit | Num | Bool) => None
  | CursorT(OnDelim(_k, Before), Parenthesized(ty1)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(ty1)))
  | CursorT(OnDelim(_k, Before), List(ty1)) =>
    // _k == 1
    Some(ListZ(place_after(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), Parenthesized(erase(zty1))))
    }
  | ListZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), List(erase(zty1))))
    };

let move_cursor_right_operator =
  fun
  | (After, _) => None
  | (Before, op) => Some((After, op));

let rec move_cursor_right = (zty: t): option(t) =>
  move_cursor_right_opseq(zty)
and move_cursor_right_opseq = opseq =>
  ZOpSeq.move_cursor_right(
    ~move_cursor_right_operand,
    ~move_cursor_right_operator,
    ~place_before_operand,
    ~place_before_operator,
    ~erase_operand,
    ~erase_operator,
    opseq,
  )
and move_cursor_right_operand =
  fun
  | zoperand when is_after_operand(zoperand) => None
  | CursorT(OnText(_) | Staging(_), _) => None
  | CursorT(OnDelim(k, Before), ty) =>
    Some(CursorT(OnDelim(k, After), ty))
  | CursorT(OnDelim(_, After), Hole | Unit | Num | Bool) => None
  | CursorT(OnDelim(_k, After), Parenthesized(ty1)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(ty1)))
  | CursorT(OnDelim(_k, After), List(ty1)) =>
    // _k == 0
    Some(ListZ(place_before(ty1)))
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None =>
      Some(CursorT(OnDelim(1, Before), Parenthesized(erase(zty1))))
    }
  | ListZ(zty1) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(1, Before), List(erase(zty1))))
    };
