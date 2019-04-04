open SemanticsCommon;

type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

type t =
  | CursorTO(outer_cursor, UHTyp.t_outer)
  | CursorTI(inner_cursor, UHTyp.t_inner)
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround);

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorTO(_, ty) => TO(ty)
  | CursorTI(_, ty) => TI(ty)
  | ParenthesizedZ(zty1) => TI(Parenthesized(erase(zty1)))
  | ListZ(zty1) => TI(List(erase(zty1)))
  | OpSeqZ(skel, zty1, surround) =>
    let uty1 = erase(zty1);
    let opseq = OperatorSeq.opseq_of_exp_and_surround(uty1, surround);
    TI(OpSeq(skel, opseq));
  };

let rec is_before = (zty: t): bool =>
  switch (zty) {
  /* leaf nodes */
  | CursorTO(Char(0), Hole)
  | CursorTO(Char(0), Unit)
  | CursorTO(Char(0), Num)
  | CursorTO(Char(0), Bool) => true
  | CursorTO(Char(_), _) => false
  /* branch nodes */
  | CursorTI(AfterChild(0), Parenthesized(_)) => true
  | CursorTI(AfterChild(0), List(_)) => true
  | CursorTI(AfterChild(_), _) => false
  | CursorTI(BeforeChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  /* leaf nodes */
  | CursorTO(Char(1), Hole)
  | CursorTO(Char(1), Unit) /* TODO is this right? */
  | CursorTO(Char(3), Num)
  | CursorTO(Char(4), Bool) => true
  | CursorTO(Char(_), _) => false
  /* branch nodes */
  | CursorTI(BeforeChild(2), Parenthesized(_)) => true
  | CursorTI(BeforeChild(2), List(_)) => true
  | CursorTI(BeforeChild(_), _) => false
  | CursorTI(AfterChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  /* leaf nodes */
  | TO(Hole as uty_o)
  | TO(Unit as uty_o)
  | TO(Num as uty_o)
  | TO(Bool as uty_o) => CursorTO(Char(0), uty_o)
  /* branch nodes */
  | TI(Parenthesized(_) as uty_i)
  | TI(List(_) as uty_i) => CursorTI(AfterChild(0), uty_i)
  | TI(OpSeq(skel, seq)) =>
    let (uty, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zty = place_before(uty);
    OpSeqZ(skel, zty, surround);
  };

let rec place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  /* leaf nodes */
  | TO(Hole as uty_o) => CursorTO(Char(1), uty_o)
  | TO(Unit as uty_o) => CursorTO(Char(1), uty_o) /* TODO is this right? */
  | TO(Num as uty_o) => CursorTO(Char(3), uty_o)
  | TO(Bool as uty_o) => CursorTO(Char(4), uty_o)
  /* branch nodes */
  | TI(Parenthesized(_) as uty_i) => CursorTI(BeforeChild(2), uty_i)
  | TI(List(_) as uty_i) => CursorTI(BeforeChild(2), uty_i)
  | TI(OpSeq(skel, seq)) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };
