open GeneralUtil;
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

let children = (utyi: UHTyp.t_inner): list(int) =>
  switch (utyi) {
  | Parenthesized(_) => [0]
  | OpSeq(_, seq) => range(OperatorSeq.seq_length(seq))
  | List(_) => [0]
  };

let children_following_delimiters = (utyi: UHTyp.t_inner): list(int) =>
  switch (utyi) {
  | Parenthesized(_) => [0]
  | OpSeq(_, seq) => range(~lo=1, OperatorSeq.seq_length(seq))
  | List(_) => [0]
  };

let has_closing_delimiter = (utyi: UHTyp.t_inner): bool =>
  switch (utyi) {
  | Parenthesized(_)
  | List(_) => true
  | OpSeq(_, _) => false
  };

let valid_inner_cursors = (utyi: UHTyp.t_inner): list(inner_cursor) => {
  let before_child_positions =
    children_following_delimiters(utyi)
    |> List.map(k => [BeforeChild(k, Before), BeforeChild(k, After)])
    |> List.flatten;
  let closing_delimiter_positions =
    has_closing_delimiter(utyi)
      ? [ClosingDelimiter(Before), ClosingDelimiter(After)] : [];
  before_child_positions @ closing_delimiter_positions;
};

let valid_outer_cursors = (utyo: UHTyp.t_outer): list(outer_cursor) =>
  range(UHTyp.t_outer_length(utyo)) |> List.map(j => Char(j));

let is_valid_inner_cursor =
    (inner_cursor: inner_cursor, utyi: UHTyp.t_inner): bool =>
  contains(valid_inner_cursors(utyi), inner_cursor);

let is_valid_outer_cursor =
    (outer_cursor: outer_cursor, utyo: UHTyp.t_outer): bool =>
  contains(valid_outer_cursors(utyo), outer_cursor);

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
  | CursorTO(Char(j), _) => j === 0
  /* inner nodes */
  | CursorTI(inner_cursor, Parenthesized(_))
  | CursorTI(inner_cursor, List(_)) =>
    inner_cursor === BeforeChild(0, Before)
  | CursorTI(_, OpSeq(_, _)) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  /* outer nodes */
  | CursorTO(Char(j), utyo) => j === UHTyp.t_outer_length(utyo)
  /* inner nodes */
  | CursorTI(inner_cursor, Parenthesized(_))
  | CursorTI(inner_cursor, List(_)) =>
    inner_cursor === ClosingDelimiter(After)
  | CursorTI(_, OpSeq(_, _)) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | TO(utyo) => CursorTO(Char(0), utyo)
  /* inner nodes */
  | TI(Parenthesized(_) as utyi)
  | TI(List(_) as utyi) => CursorTI(BeforeChild(0, Before), utyi)
  | TI(OpSeq(skel, seq)) =>
    let (uty, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zty = place_before(uty);
    OpSeqZ(skel, zty, surround);
  };

let rec place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  /* outer nodes */
  | TO(utyo) => CursorTO(Char(UHTyp.t_outer_length(utyo)), utyo)
  /* inner nodes */
  | TI(Parenthesized(_) as utyi)
  | TI(List(_) as utyi) => CursorTI(ClosingDelimiter(After), utyi)
  | TI(OpSeq(skel, seq)) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };

let place_cursor = (cursor: cursor_pos, uty: UHTyp.t): option(t) =>
  switch (cursor, uty) {
  | (O(outer_cursor), TO(utyo)) =>
    is_valid_outer_cursor(outer_cursor, utyo)
      ? Some(CursorTO(outer_cursor, utyo)) : None
  | (I(inner_cursor), TI(utyi)) =>
    is_valid_inner_cursor(inner_cursor, utyi)
      ? Some(CursorTI(inner_cursor, utyi)) : None
  | (O(_), TI(_))
  | (I(_), TO(_)) => None
  };
