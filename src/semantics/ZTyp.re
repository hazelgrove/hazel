open GeneralUtil;
open SemanticsCommon;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

[@deriving sexp]
type t =
  | CursorT(cursor_position, UHTyp.t)
  /* zipper cases */
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

let valid_cursors = (uty: UHTyp.t): list(cursor_position) =>
  switch (uty) {
  | Hole
  | Unit
  | Num
  | Bool => delim_cursors(1)
  | Parenthesized(_)
  | List(_) => delim_cursors(2)
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.map(k => k + 1)
    |> List.map(k => delim_cursors_k(k))
    |> List.flatten
  };

let is_valid_cursor = (cursor: cursor_position, uty: UHTyp.t): bool =>
  valid_cursors(uty) |> contains(cursor);

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorT(_, ty) => ty
  | ParenthesizedZ(zty1) => Parenthesized(erase(zty1))
  | ListZ(zty1) => List(erase(zty1))
  | OpSeqZ(skel, zty1, surround) =>
    let uty1 = erase(zty1);
    let opseq = OperatorSeq.opseq_of_exp_and_surround(uty1, surround);
    OpSeq(skel, opseq);
  };

let rec is_before = (zty: t): bool =>
  switch (zty) {
  /* outer nodes */
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Num)
  | CursorT(cursor, Bool) => cursor == OnDelim(0, Before)
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | CursorT(_, OpSeq(_, _)) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  /* outer nodes */
  | CursorT(cursor, Hole)
  | CursorT(cursor, Unit)
  | CursorT(cursor, Num)
  | CursorT(cursor, Bool) => cursor == OnDelim(0, After)
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | CursorT(_, OpSeq(_, _)) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | Hole
  | Unit
  | Num
  | Bool
  /* inner nodes */
  | Parenthesized(_)
  | List(_) => CursorT(OnDelim(0, Before), uty)
  | OpSeq(skel, seq) =>
    let (uty, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zty = place_before(uty);
    OpSeqZ(skel, zty, surround);
  };

let rec place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | Hole
  | Unit
  | Num
  | Bool => CursorT(OnDelim(0, After), uty)
  /* inner nodes */
  | Parenthesized(_)
  | List(_) => CursorT(OnDelim(1, After), uty)
  | OpSeq(skel, seq) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };

let place_cursor = (cursor: cursor_position, uty: UHTyp.t): option(t) =>
  is_valid_cursor(cursor, uty) ? Some(CursorT(cursor, uty)) : None;

let rec cursor_on_opseq = (zty: t): bool =>
  switch (zty) {
  | CursorT(_, OpSeq(_, _)) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(zty) => cursor_on_opseq(zty)
  | ListZ(zty) => cursor_on_opseq(zty)
  | OpSeqZ(_, zty, _) => cursor_on_opseq(zty)
  };

let node_positions = (uty: UHTyp.t): list(node_position) =>
  switch (uty) {
  /* outer nodes */
  | Hole
  | Unit
  | Num
  | Bool => node_positions(valid_cursors(uty))
  /* inner nodes */
  | Parenthesized(_)
  | List(_) =>
    node_positions(delim_cursors_k(0))
    @ [Deeper(0)]
    @ node_positions(delim_cursors_k(1))
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.fold_left(
         (lstSoFar, i) =>
           switch (lstSoFar) {
           | [] => [Deeper(i)]
           | [_, ..._] =>
             lstSoFar @ node_positions(delim_cursors_k(i)) @ [Deeper(i)]
           },
         [],
       )
  };

let node_position_of_t = (zty: t): node_position =>
  switch (zty) {
  | CursorT(cursor, _) => On(cursor)
  | ParenthesizedZ(_) => Deeper(0)
  | ListZ(_) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  };

let rec cursor_node_type = (zty: t): node_type =>
  switch (zty) {
  /* outer nodes */
  | CursorT(_, Hole)
  | CursorT(_, Unit)
  | CursorT(_, Num)
  | CursorT(_, Bool) => Outer
  /* inner nodes */
  | CursorT(_, Parenthesized(_))
  | CursorT(_, List(_))
  | CursorT(_, OpSeq(_, _)) => Inner
  /* zipper */
  | ParenthesizedZ(zty1) => cursor_node_type(zty1)
  | ListZ(zty1) => cursor_node_type(zty1)
  | OpSeqZ(_, zty1, _) => cursor_node_type(zty1)
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
    switch (seq |> OperatorSeq.split(k - 1)) {
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
      let k = OperatorSeq.surround_prefix_length(surround);
      let seq = OperatorSeq.opseq_of_exp_and_surround(erase(zty1), surround);
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
    switch (seq |> OperatorSeq.split(k)) {
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
      let k = OperatorSeq.surround_prefix_length(surround);
      let seq = OperatorSeq.opseq_of_exp_and_surround(erase(zty1), surround);
      Some(CursorT(OnDelim(k + 1, Before), OpSeq(skel, seq)));
    }
  };
