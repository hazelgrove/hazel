open GeneralUtil;
open SemanticsCommon;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

[@deriving sexp]
type t =
  | CursorT(cursor_pos, UHTyp.t)
  /* zipper cases */
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

let valid_cursors = (uty: UHTyp.t): list(cursor_pos) =>
  switch (uty) {
  /* outer nodes */
  | Hole => outer_cursors(1)
  | Unit => outer_cursors(1)
  | Num => outer_cursors(3)
  | Bool => outer_cursors(4)
  /* inner nodes */
  | Parenthesized(_) => inner_cursors(2)
  | List(_) => inner_cursors(2)
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.map(k => k + 1)
    |> List.map(k => inner_cursors_k(k))
    |> List.flatten
  };

let is_valid_cursor = (cursor: cursor_pos, uty: UHTyp.t): bool =>
  contains(valid_cursors(uty), cursor);

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
  | CursorT(cursor, Bool) => cursor == outer_cursor(0)
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == inner_cursor(0, Before)
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
  | CursorT(cursor, Hole) => cursor == outer_cursor(1)
  | CursorT(cursor, Unit) => cursor == outer_cursor(1)
  | CursorT(cursor, Num) => cursor == outer_cursor(3)
  | CursorT(cursor, Bool) => cursor == outer_cursor(4)
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == inner_cursor(1, After)
  | CursorT(_, OpSeq(_, _)) => false
  /* zipper cases */
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
  | Bool => CursorT(outer_cursor(0), uty)
  /* inner nodes */
  | Parenthesized(_)
  | List(_) => CursorT(inner_cursor(0, Before), uty)
  | OpSeq(skel, seq) =>
    let (uty, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zty = place_before(uty);
    OpSeqZ(skel, zty, surround);
  };

let rec place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | Hole => CursorT(outer_cursor(1), uty)
  | Unit => CursorT(outer_cursor(1), uty)
  | Num => CursorT(outer_cursor(3), uty)
  | Bool => CursorT(outer_cursor(4), uty)
  /* inner nodes */
  | Parenthesized(_)
  | List(_) => CursorT(inner_cursor(1, After), uty)
  | OpSeq(skel, seq) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };

let place_cursor = (cursor: cursor_pos, uty: UHTyp.t): option(t) =>
  is_valid_cursor(cursor, uty) ? Some(CursorT(cursor, uty)) : None;

let rec cursor_on_opseq = (zty: t): bool =>
  switch (zty) {
  | CursorT(_, OpSeq(_, _)) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(zty) => cursor_on_opseq(zty)
  | ListZ(zty) => cursor_on_opseq(zty)
  | OpSeqZ(_, zty, _) => cursor_on_opseq(zty)
  };
