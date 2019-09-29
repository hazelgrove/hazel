open GeneralUtil;
open SemanticsCommon;

[@deriving sexp]
type t = zopseq
and zopseq = OpSeqZ.t(UHTyp.operand, UHTyp.operator, zoperand, zoperator)
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
  valid_cursors(uty) |> contains(cursor);

let rec erase = (zty: t): UHTyp.t => erase_opseq(zty)
and erase_opseq = OpSeqZ.erase(~erase_operand, ~erase_operator)
and erase_operand =
  fun
  | CursorT(_, operand) => operand
  | ParenthesizedZ(zty) => Parenthesized(erase(zty))
  | ListZ(zty) => List(erase(zty));

let rec is_before = (zty: t): bool => is_before_opseq(zty)
and is_before_opseq = OpSeqZ.is_before(~is_before_operand)
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
and is_after_opseq = OpSeqZ.is_after(~is_after_operand)
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
and place_before_opseq = OpSeqZ.place_before(~place_before_operand)
and place_before_operand =
  fun
  | (Hole | Unit | Num | Bool | Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(0, Before), uty);

let rec place_after = (uty: UHTyp.t): t => place_after_opseq(uty)
and place_after_opseq = OpSeqZ.place_after(~place_after_operand)
and place_after_operand =
  fun
  | (Hole | Unit | Num | Bool) as operand =>
    CursorT(OnDelim(0, After), operand)
  | (Parenthesized(_) | List(_)) as operand =>
    CursorT(OnDelim(1, After), operand);

let place_cursor = (cursor: cursor_position, operand: UHTyp.operand): option(zoperand) =>
  is_valid_cursor(cursor, operand) ? Some(CursorT(cursor, operand)) : None;

let rec cursor_on_opseq = (zty: t): bool =>
  switch (zty) {
  | CursorT(_, OpSeq(_, _)) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(zty) => cursor_on_opseq(zty)
  | ListZ(zty) => cursor_on_opseq(zty)
  | OpSeqZ(_, zty, _) => cursor_on_opseq(zty)
  };

let rec move_cursor_left = (zty: t): option(t) =>
  switch (zty) {
  | _ when is_before(zty) => None
  | CursorT(Staging(_), _) => None
  | CursorT(OnDelim(k, After), ty) =>
    Some(CursorT(OnDelim(k, Before), ty))
  | CursorT(OnDelim(_, Before), Hole | Unit | Num | Bool) => None
  | CursorT(OnDelim(_k, Before), Parenthesized(ty1)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(ty1)))
  | CursorT(OnDelim(_k, Before), List(ty1)) =>
    // _k == 1
    Some(ListZ(place_after(ty1)))
  | CursorT(OnDelim(k, Before), OpSeq(skel, seq)) =>
    switch (seq |> Seq.split(k - 1)) {
    | None => None // should never happen
    | Some((ty1, surround)) =>
      Some(OpSeqZ(skel, place_after(ty1), surround))
    }
  | CursorT(OnText(_), _) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ParenthesizedZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), Parenthesized(erase(zty1))))
    }
  | ListZ(zty1) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(ListZ(zty1))
    | None => Some(CursorT(OnDelim(0, After), List(erase(zty1))))
    }
  | OpSeqZ(skel, zty1, surround) =>
    switch (move_cursor_left(zty1)) {
    | Some(zty1) => Some(OpSeqZ(skel, zty1, surround))
    | None =>
      let k = Seq.surround_prefix_length(surround);
      let seq = Seq.t_of_operand_and_surround(erase(zty1), surround);
      Some(CursorT(OnDelim(k, After), OpSeq(skel, seq)));
    }
  };

let rec move_cursor_right = (zty: t): option(t) =>
  switch (zty) {
  | _ when is_after(zty) => None
  | CursorT(Staging(_), _) => None
  | CursorT(OnDelim(k, Before), ty) =>
    Some(CursorT(OnDelim(k, After), ty))
  | CursorT(OnDelim(_, After), Hole | Unit | Num | Bool) => None
  | CursorT(OnDelim(_k, After), Parenthesized(ty1)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(ty1)))
  | CursorT(OnDelim(_k, After), List(ty1)) =>
    // _k == 0
    Some(ListZ(place_before(ty1)))
  | CursorT(OnDelim(k, After), OpSeq(skel, seq)) =>
    switch (seq |> Seq.split(k)) {
    | None => None // should never happen
    | Some((ty1, surround)) =>
      Some(OpSeqZ(skel, place_before(ty1), surround))
    }
  | CursorT(OnText(_), _) =>
    // invalid cursor position
    None
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
    }
  | OpSeqZ(skel, zty1, surround) =>
    switch (move_cursor_right(zty1)) {
    | Some(zty1) => Some(OpSeqZ(skel, zty1, surround))
    | None =>
      let k = Seq.surround_prefix_length(surround);
      let seq = Seq.t_of_operand_and_surround(erase(zty1), surround);
      Some(CursorT(OnDelim(k + 1, Before), OpSeq(skel, seq)));
    }
  };
