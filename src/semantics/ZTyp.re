type cursor_side = SemanticsCommon.cursor_side;

type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

type t =
  | CursorT(cursor_side, UHTyp.t)
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

let place_Before = (uty: UHTyp.t): t =>
  switch (uty) {
  | UHTyp.Hole
  | UHTyp.Parenthesized(_)
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool
  | UHTyp.List(_) => CursorT(Before, uty)
  | UHTyp.OpSeq(skel, seq) =>
    let (uty0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, CursorT(Before, uty0), surround);
  };

let place_After = (uty: UHTyp.t): t =>
  switch (uty) {
  | UHTyp.Hole
  | UHTyp.Parenthesized(_)
  | UHTyp.Unit
  | UHTyp.Num
  | UHTyp.Bool
  | UHTyp.List(_) => CursorT(After, uty)
  | UHTyp.OpSeq(skel, seq) =>
    let (uty0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, CursorT(After, uty0), surround);
  };

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorT(_, ty) => ty
  | ParenthesizedZ(zty1) => UHTyp.Parenthesized(erase(zty1))
  | ListZ(zty1) => UHTyp.List(erase(zty1))
  | OpSeqZ(skel, zty1, surround) =>
    let uty1 = erase(zty1);
    UHTyp.OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(uty1, surround));
  };

let rec cursor_at_end = (zty: t): bool =>
  switch (zty) {
  | CursorT(After, _) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(_) => false
  | ListZ(t) => false
  | OpSeqZ(_, zty1, OperatorSeq.EmptySuffix(_)) => true
  | OpSeqZ(_, _, _) => false
  };