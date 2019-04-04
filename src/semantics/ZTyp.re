type cursor_pos = SemanticsCommon.cursor_pos;

type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

type t =
  | CursorT(cursor_pos, UHTyp.t)
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

let place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  | Hole
  | Parenthesized(_)
  | Unit
  | Num
  | Bool
  | List(_) => CursorT(Before, uty)
  | OpSeq(skel, seq) =>
    let (uty0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, CursorT(Before, uty0), surround);
  };

let place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  | Hole
  | Parenthesized(_)
  | Unit
  | Num
  | Bool
  | List(_) => CursorT(After, uty)
  | OpSeq(skel, seq) =>
    let (uty0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, CursorT(After, uty0), surround);
  };

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorT(_, ty) => ty
  | ParenthesizedZ(zty1) => Parenthesized(erase(zty1))
  | ListZ(zty1) => List(erase(zty1))
  | OpSeqZ(skel, zty1, surround) =>
    let uty1 = erase(zty1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(uty1, surround));
  };

let rec is_before = (zty: t): bool =>
  switch (zty) {
  | CursorT(Before, _) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  | CursorT(After, _) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };
