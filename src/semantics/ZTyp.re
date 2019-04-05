open SemanticsCommon;

type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

type t =
  | CursorTO(outer_cursor, UHTyp.t_outer)
  | CursorTI(inner_cursor, UHTyp.t_inner)
  /* zipper cases */
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
  /* outer nodes */
  | CursorTO(Char(j), Hole)
  | CursorTO(Char(j), Unit)
  | CursorTO(Char(j), Num)
  | CursorTO(Char(j), Bool) => j === 0
  /* inner nodes */
  | CursorTI(AfterChild(k), Parenthesized(_))
  | CursorTI(AfterChild(k), List(_)) => k === 0
  | CursorTI(AfterChild(_), OpSeq(_, _)) => false
  | CursorTI(BeforeChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  /* outer nodes */
  | CursorTO(Char(j), Hole) => j === 1
  | CursorTO(Char(j), Unit) => j === 1 /* TODO is this right? */
  | CursorTO(Char(j), Num) => j === 3
  | CursorTO(Char(j), Bool) => j === 4
  /* inner nodes */
  | CursorTI(BeforeChild(k), Parenthesized(_))
  | CursorTI(BeforeChild(k), List(_)) => k === 2
  | CursorTI(BeforeChild(_), OpSeq(_, _)) => false
  | CursorTI(AfterChild(_), _) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | TO(Hole as uty_o)
  | TO(Unit as uty_o)
  | TO(Num as uty_o)
  | TO(Bool as uty_o) => CursorTO(Char(0), uty_o)
  /* inner nodes */
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
  /* outer nodes */
  | TO(Hole as uty_o) => CursorTO(Char(1), uty_o)
  | TO(Unit as uty_o) => CursorTO(Char(1), uty_o) /* TODO is this right? */
  | TO(Num as uty_o) => CursorTO(Char(3), uty_o)
  | TO(Bool as uty_o) => CursorTO(Char(4), uty_o)
  /* inner nodes */
  | TI(Parenthesized(_) as uty_i) => CursorTI(BeforeChild(2), uty_i)
  | TI(List(_) as uty_i) => CursorTI(BeforeChild(2), uty_i)
  | TI(OpSeq(skel, seq)) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };
