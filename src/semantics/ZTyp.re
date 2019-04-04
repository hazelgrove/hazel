type cursor_pos = SemanticsCommon.cursor_pos;

type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

type t =
  | CursorT(cursor_pos, UHTyp.t)
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

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
  /* leaf nodes */
  | CursorT(InLeaf(0), Hole)
  | CursorT(InLeaf(0), Unit)
  | CursorT(InLeaf(0), Num)
  | CursorT(InLeaf(0), Bool) => true
  | CursorT(InLeaf(_), _) => false
  /* branch nodes */
  | CursorT(AfterChild(0), Parenthesized(_)) => true
  | CursorT(AfterChild(0), List(_)) => true
  | CursorT(AfterChild(_), _) => false
  | CursorT(BeforeChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  /* leaf nodes */
  | CursorT(InLeaf(1), Hole)
  | CursorT(InLeaf(1), Unit) /* TODO is this right? */
  | CursorT(InLeaf(3), Num)
  | CursorT(InLeaf(4), Bool) => true
  | CursorT(InLeaf(_), _) => false
  /* branch nodes */
  | CursorT(BeforeChild(2), Parenthesized(_)) => true
  | CursorT(BeforeChild(2), List(_)) => true
  | CursorT(BeforeChild(_), _) => false
  | CursorT(AfterChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  /* leaf nodes */
  | Hole
  | Unit
  | Num
  | Bool => CursorT(InLeaf(0), uty)
  /* branch nodes */
  | Parenthesized(_)
  | List(_) => CursorT(AfterChild(0), uty)
  | OpSeq(skel, seq) =>
    let (uty, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zty = place_before(uty);
    OpSeqZ(skel, zty, surround);
  };

let rec place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  /* leaf nodes */
  | Hole => CursorT(InLeaf(1), uty)
  | Unit => CursorT(InLeaf(1), uty) /* TODO is this right? */
  | Num => CursorT(InLeaf(3), uty)
  | Bool => CursorT(InLeaf(4), uty)
  /* branch nodes */
  | Parenthesized(_) => CursorT(BeforeChild(2), uty)
  | List(_) => CursorT(BeforeChild(2), uty)
  | OpSeq(skel, seq) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };
