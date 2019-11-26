open GeneralUtil;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
[@deriving sexp]
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
[@deriving sexp]
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

[@deriving sexp]
type t =
  | CursorT(CursorPosition.t, UHTyp.t)
  /* zipper cases */
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround)
  /* in the pattern */
  | ForallZP(ZTPat.t, UHTyp.t)
  /* in the type */
  | ForallZT(TPat.t, t);

let valid_cursors = (uty: UHTyp.t): list(CursorPosition.t) =>
  switch (uty) {
  | TVar(_)
  | Forall(_, _)
  | Hole
  | Unit
  | Num
  | Bool => CursorPosition.delim_cursors(1)
  | Parenthesized(_)
  | List(_) => CursorPosition.delim_cursors(2)
  | OpSeq(_, seq) =>
    range(~lo=1, OperatorSeq.seq_length(seq))
    |> List.map(k => CursorPosition.delim_cursors_k(k))
    |> List.flatten
  };

let is_valid_cursor = (cursor: CursorPosition.t, uty: UHTyp.t): bool =>
  valid_cursors(uty) |> contains(cursor);

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorT(_, ty) => ty
  | ParenthesizedZ(zty1) => Parenthesized(erase(zty1))
  | ListZ(zty1) => List(erase(zty1))
  | ForallZP(ztpat, uhtyp1) => Forall(ZTPat.erase(ztpat), uhtyp1)
  | ForallZT(tpat, zty1) => Forall(tpat, erase(zty1))
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
  | CursorT(_, TVar(_, _) | Forall(_, _)) =>
    raise(Failure("unimplemented"))
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(0, Before)
  | CursorT(_, OpSeq(_, _)) => false
  /* zipper cases */
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | ForallZP(_) => false
  | ForallZT(_) => false
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
  | CursorT(_, TVar(_, _) | Forall(_, _)) =>
    raise(Failure("unimplemented"))
  /* inner nodes */
  | CursorT(cursor, Parenthesized(_))
  | CursorT(cursor, List(_)) => cursor == OnDelim(1, After)
  | CursorT(_, OpSeq(_, _)) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | ForallZP(_) => false
  | ForallZT(_) => false
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
  | TVar(_, _)
  | Forall(_, _) => raise(Failure("unimplemented"))
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
  | TVar(_, _)
  | Forall(_, _) => raise(Failure("unimplemented"))
  /* inner nodes */
  | Parenthesized(_)
  | List(_) => CursorT(OnDelim(1, After), uty)
  | OpSeq(skel, seq) =>
    let (uty, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zty = place_after(uty);
    OpSeqZ(skel, zty, surround);
  };

let place_cursor = (cursor: CursorPosition.t, uty: UHTyp.t): option(t) =>
  is_valid_cursor(cursor, uty) ? Some(CursorT(cursor, uty)) : None;

let rec cursor_on_opseq = (zty: t): bool =>
  switch (zty) {
  | CursorT(_, OpSeq(_, _)) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(zty) => cursor_on_opseq(zty)
  | ForallZP(_, _)
  | ForallZT(_, _) => raise(Failure("unimplemented"))
  | ListZ(_zty) =>
    /*
     c: what is going on here? didn't type check */
    /* cursor_o;  n_opseq(zty) */
    raise(Failure("unimplemented"))
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
  | CursorT(OnDelim(_, Before), TVar(_, _) | Forall(_, _))
  | ForallZP(_, _)
  | ForallZT(_, _) => raise(Failure("unimplemented"))
  };

let rec move_cursor_right = (zty: t): option(t) =>
  switch (zty) {
  | _ when is_after(zty) => None
  | CursorT(Staging(_), _) => None
  | CursorT(OnDelim(k, Before), ty) =>
    Some(CursorT(OnDelim(k, After), ty))
  | CursorT(OnDelim(_, After), Hole | Unit | Num | Bool) => None
  | CursorT(OnDelim(_, After), TVar(_, _) | Forall(_, _))
  | ForallZP(_, _)
  | ForallZT(_, _) => raise(Failure("unimplemented"))
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

let new_ForallHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (hole, u_gen) = UHTyp.new_ForallHole(u_gen);
  (CursorT(OnText(0), hole), u_gen);
};
