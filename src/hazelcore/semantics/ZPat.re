open GeneralUtil;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHPat.t, UHPat.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHPat.t, UHPat.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHPat.t, UHPat.op);

[@deriving sexp]
type t =
  | CursorP(CursorPosition.t, UHPat.t)
  /* zipper cases */
  | ParenthesizedZ(t)
  | OpSeqZ(UHPat.skel_t, t, opseq_surround)
  | InjZ(ErrStatus.t, InjSide.t, t);

exception SkelInconsistentWithOpSeq;

let valid_cursors = (p: UHPat.t): list(CursorPosition.t) =>
  switch (p) {
  | EmptyHole(_) => CursorPosition.delim_cursors(1)
  | Wild(_) => CursorPosition.delim_cursors(1)
  | Var(_, _, x) => CursorPosition.text_cursors(Var.length(x))
  | NumLit(_, n) => CursorPosition.text_cursors(num_digits(n))
  | BoolLit(_, b) => CursorPosition.text_cursors(b ? 4 : 5)
  | ListNil(_) => CursorPosition.delim_cursors(1)
  | Inj(_, _, _) => CursorPosition.delim_cursors(2)
  | Parenthesized(_) => CursorPosition.delim_cursors(2)
  | OpSeq(_, seq) =>
    range(~lo=1, OperatorSeq.seq_length(seq))
    |> List.map(k => CursorPosition.delim_cursors_k(k))
    |> List.flatten
  };

let is_valid_cursor = (cursor: CursorPosition.t, p: UHPat.t): bool =>
  valid_cursors(p) |> contains(cursor);

let bidelimit = (zp: t): t =>
  switch (zp) {
  | CursorP(_, p) =>
    if (UHPat.bidelimited(p)) {
      zp;
    } else {
      ParenthesizedZ(zp);
    }
  | ParenthesizedZ(_)
  | InjZ(_, _, _) => zp
  | OpSeqZ(_, _, _) => ParenthesizedZ(zp)
  };

let rec set_err_status_t = (err: ErrStatus.t, zp: t): t =>
  switch (zp) {
  | CursorP(cursor, p) =>
    let p = UHPat.set_err_status_t(err, p);
    CursorP(cursor, p);
  | ParenthesizedZ(zp1) => ParenthesizedZ(set_err_status_t(err, zp1))
  | InjZ(_, inj_side, zp1) => InjZ(err, inj_side, zp1)
  | OpSeqZ(skel, zp_n, surround) =>
    let (skel, zp_n, surround) =
      set_err_status_opseq(err, skel, zp_n, surround);
    OpSeqZ(skel, zp_n, surround);
  }
and set_err_status_opseq =
    (err: ErrStatus.t, skel: UHPat.skel_t, zp_n: t, surround: opseq_surround)
    : (UHPat.skel_t, t, opseq_surround) =>
  switch (skel) {
  | Placeholder(m) =>
    if (m === OperatorSeq.surround_prefix_length(surround)) {
      let zp_n = set_err_status_t(err, zp_n);
      (skel, zp_n, surround);
    } else {
      switch (OperatorSeq.surround_nth(m, surround)) {
      | None => raise(SkelInconsistentWithOpSeq)
      | Some(p_m) =>
        let p_m = UHPat.set_err_status_t(err, p_m);
        switch (OperatorSeq.surround_update_nth(m, surround, p_m)) {
        | None => raise(SkelInconsistentWithOpSeq)
        | Some(surround) => (skel, zp_n, surround)
        };
      };
    }
  | BinOp(_, op, skel1, skel2) => (
      BinOp(err, op, skel1, skel2),
      zp_n,
      surround,
    )
  };

let rec make_t_inconsistent = (u_gen: MetaVarGen.t, zp: t): (t, MetaVarGen.t) =>
  switch (zp) {
  | CursorP(cursor, p) =>
    let (p, u_gen) = UHPat.make_t_inconsistent(u_gen, p);
    (CursorP(cursor, p), u_gen);
  | InjZ(InHole(TypeInconsistent, _), _, _) => (zp, u_gen)
  | InjZ(
      NotInHole | InHole(WrongLength | InconsistentBranches(_), _),
      inj_side,
      zp1,
    ) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (InjZ(InHole(TypeInconsistent, u), inj_side, zp1), u_gen);
  | ParenthesizedZ(zp1) =>
    let (zp1, u_gen) = make_t_inconsistent(u_gen, zp1);
    (ParenthesizedZ(zp1), u_gen);
  | OpSeqZ(skel, zp_n, surround) =>
    let (skel, zp_n, surround, u_gen) =
      make_opseq_inconsistent(u_gen, skel, zp_n, surround);
    (OpSeqZ(skel, zp_n, surround), u_gen);
  }
and make_opseq_inconsistent =
    (
      u_gen: MetaVarGen.t,
      skel: UHPat.skel_t,
      zp_n: t,
      surround: opseq_surround,
    )
    : (UHPat.skel_t, t, opseq_surround, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(m) =>
    if (m === OperatorSeq.surround_prefix_length(surround)) {
      let (zp_n, u_gen) = make_t_inconsistent(u_gen, zp_n);
      (skel, zp_n, surround, u_gen);
    } else {
      switch (OperatorSeq.surround_nth(m, surround)) {
      | None => raise(SkelInconsistentWithOpSeq)
      | Some(p_m) =>
        let (p_m, u_gen) = UHPat.make_t_inconsistent(u_gen, p_m);
        switch (OperatorSeq.surround_update_nth(m, surround, p_m)) {
        | None => raise(SkelInconsistentWithOpSeq)
        | Some(surround) => (skel, zp_n, surround, u_gen)
        };
      };
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => (
      skel,
      zp_n,
      surround,
      u_gen,
    )
  | BinOp(NotInHole, op, skel1, skel2)
  | BinOp(InHole(WrongLength | InconsistentBranches(_), _), op, skel1, skel2) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (
      BinOp(InHole(TypeInconsistent, u), op, skel1, skel2),
      zp_n,
      surround,
      u_gen,
    );
  };

let rec erase = (zp: t): UHPat.t =>
  switch (zp) {
  | CursorP(_, p) => p
  | InjZ(err, inj_side, zp1) => Inj(err, inj_side, erase(zp1))
  | ParenthesizedZ(zp) => Parenthesized(erase(zp))
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround));
  };

let rec is_before = (zp: t): bool =>
  switch (zp) {
  /* outer nodes - delimiter */
  | CursorP(cursor, EmptyHole(_))
  | CursorP(cursor, Wild(_))
  | CursorP(cursor, ListNil(_)) => cursor == OnDelim(0, Before)
  /* outer nodes - text */
  | CursorP(cursor, Var(_, _, _))
  | CursorP(cursor, NumLit(_, _))
  | CursorP(cursor, BoolLit(_, _)) => cursor == OnText(0)
  /* inner nodes */
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == OnDelim(0, Before)
  | CursorP(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptyPrefix(_)) => is_before(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zp: t): bool =>
  switch (zp) {
  /* outer nodes - delimiter */
  | CursorP(cursor, EmptyHole(_))
  | CursorP(cursor, Wild(_))
  | CursorP(cursor, ListNil(_)) => cursor == OnDelim(0, After)
  /* outer nodes - text */
  | CursorP(cursor, Var(_, _, x)) => cursor == OnText(Var.length(x))
  | CursorP(cursor, NumLit(_, n)) => cursor == OnText(num_digits(n))
  | CursorP(cursor, BoolLit(_, b)) => cursor == OnText(b ? 4 : 5)
  /* inner nodes */
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == OnDelim(1, After)
  | CursorP(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptySuffix(_)) => is_after(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes - delimiter */
  | EmptyHole(_)
  | Wild(_)
  | ListNil(_) => CursorP(OnDelim(0, Before), p)
  /* outer nodes - text */
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _) => CursorP(OnText(0), p)
  /* inner nodes */
  | Inj(_, _, _)
  | Parenthesized(_) => CursorP(OnDelim(0, Before), p)
  | OpSeq(skel, seq) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zp0 = place_before(p0);
    OpSeqZ(skel, zp0, surround);
  };

let rec place_after = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes - delimiter */
  | EmptyHole(_)
  | Wild(_)
  | ListNil(_) => CursorP(OnDelim(0, After), p)
  /* outer nodes - text */
  | Var(_, _, x) => CursorP(OnText(Var.length(x)), p)
  | NumLit(_, n) => CursorP(OnText(num_digits(n)), p)
  | BoolLit(_, b) => CursorP(OnText(b ? 4 : 5), p)
  /* inner nodes */
  | Inj(_, _, _) => CursorP(OnDelim(1, After), p)
  | Parenthesized(_) => CursorP(OnDelim(1, After), p)
  | OpSeq(skel, seq) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zp0 = place_after(p0);
    OpSeqZ(skel, zp0, surround);
  };

let place_cursor = (cursor: CursorPosition.t, p: UHPat.t): option(t) =>
  is_valid_cursor(cursor, p) ? Some(CursorP(cursor, p)) : None;

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (hole, u_gen) = UHPat.new_EmptyHole(u_gen);
  (place_before(hole), u_gen);
};

let is_inconsistent = (zp: t): bool => UHPat.is_inconsistent(erase(zp));

let rec cursor_on_opseq = (zp: t): bool =>
  switch (zp) {
  | CursorP(_, OpSeq(_, _)) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(zp) => cursor_on_opseq(zp)
  | OpSeqZ(_, zp, _) => cursor_on_opseq(zp)
  | InjZ(_, _, zp) => cursor_on_opseq(zp)
  };

let rec move_cursor_left = (zp: t): option(t) =>
  switch (zp) {
  | _ when is_before(zp) => None
  | CursorP(Staging(_), _) => None
  | CursorP(OnText(j), p) => Some(CursorP(OnText(j - 1), p))
  | CursorP(OnDelim(k, After), p) => Some(CursorP(OnDelim(k, Before), p))
  | CursorP(OnDelim(_, Before), EmptyHole(_) | Wild(_) | ListNil(_)) => None
  | CursorP(OnDelim(_k, Before), Parenthesized(p1)) =>
    // _k == 1
    Some(ParenthesizedZ(place_after(p1)))
  | CursorP(OnDelim(_k, Before), Inj(err, side, p1)) =>
    // _k == 1
    Some(InjZ(err, side, place_after(p1)))
  | CursorP(OnDelim(k, Before), OpSeq(skel, seq)) =>
    switch (seq |> OperatorSeq.split(k - 1)) {
    | None => None // should never happen
    | Some((p1, surround)) => Some(OpSeqZ(skel, place_after(p1), surround))
    }
  | CursorP(OnDelim(_, _), Var(_, _, _) | BoolLit(_, _) | NumLit(_, _)) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zp1) =>
    switch (move_cursor_left(zp1)) {
    | Some(zp1) => Some(ParenthesizedZ(zp1))
    | None => Some(CursorP(OnDelim(0, After), Parenthesized(erase(zp1))))
    }
  | InjZ(err, side, zp1) =>
    switch (move_cursor_left(zp1)) {
    | Some(zp1) => Some(InjZ(err, side, zp1))
    | None => Some(CursorP(OnDelim(0, After), Inj(err, side, erase(zp1))))
    }
  | OpSeqZ(skel, zp1, surround) =>
    switch (move_cursor_left(zp1)) {
    | Some(zp1) => Some(OpSeqZ(skel, zp1, surround))
    | None =>
      switch (surround) {
      | EmptyPrefix(_) => None
      | EmptySuffix(ExpPrefix(_, Space) | SeqPrefix(_, Space))
      | BothNonEmpty(ExpPrefix(_, Space) | SeqPrefix(_, Space), _) =>
        let k = OperatorSeq.surround_prefix_length(surround);
        let seq =
          OperatorSeq.opseq_of_exp_and_surround(erase(zp1), surround);
        switch (seq |> OperatorSeq.split(k - 1)) {
        | None => None // should never happen
        | Some((p1, surround)) =>
          Some(OpSeqZ(skel, place_after(p1), surround))
        };
      | _ =>
        let k = OperatorSeq.surround_prefix_length(surround);
        let seq =
          OperatorSeq.opseq_of_exp_and_surround(erase(zp1), surround);
        Some(CursorP(OnDelim(k, After), OpSeq(skel, seq)));
      }
    }
  };

let rec move_cursor_right = (zp: t): option(t) =>
  switch (zp) {
  | _ when is_after(zp) => None
  | CursorP(Staging(_), _) => None
  | CursorP(OnText(j), p) => Some(CursorP(OnText(j + 1), p))
  | CursorP(OnDelim(k, Before), p) => Some(CursorP(OnDelim(k, After), p))
  | CursorP(OnDelim(_, After), EmptyHole(_) | Wild(_) | ListNil(_)) => None
  | CursorP(OnDelim(_k, After), Parenthesized(p1)) =>
    // _k == 0
    Some(ParenthesizedZ(place_before(p1)))
  | CursorP(OnDelim(_k, After), Inj(err, side, p1)) =>
    // _k == 0
    Some(InjZ(err, side, place_before(p1)))
  | CursorP(OnDelim(k, After), OpSeq(skel, seq)) =>
    switch (seq |> OperatorSeq.split(k)) {
    | None => None // should never happen
    | Some((p1, surround)) =>
      Some(OpSeqZ(skel, place_before(p1), surround))
    }
  | CursorP(OnDelim(_, _), Var(_, _, _) | BoolLit(_, _) | NumLit(_, _)) =>
    // invalid cursor position
    None
  | ParenthesizedZ(zp1) =>
    switch (move_cursor_right(zp1)) {
    | Some(zp1) => Some(ParenthesizedZ(zp1))
    | None => Some(CursorP(OnDelim(1, Before), Parenthesized(erase(zp1))))
    }
  | InjZ(err, side, zp1) =>
    switch (move_cursor_right(zp1)) {
    | Some(zp1) => Some(InjZ(err, side, zp1))
    | None =>
      Some(CursorP(OnDelim(1, Before), Inj(err, side, erase(zp1))))
    }
  | OpSeqZ(skel, zp1, surround) =>
    switch (move_cursor_right(zp1)) {
    | Some(zp1) => Some(OpSeqZ(skel, zp1, surround))
    | None =>
      switch (surround) {
      | EmptySuffix(_) => None
      | EmptyPrefix(ExpSuffix(Space, _) | SeqSuffix(Space, _))
      | BothNonEmpty(_, ExpSuffix(Space, _) | SeqSuffix(Space, _)) =>
        let k = OperatorSeq.surround_prefix_length(surround);
        let seq =
          OperatorSeq.opseq_of_exp_and_surround(erase(zp1), surround);
        switch (seq |> OperatorSeq.split(k + 1)) {
        | None => None // should never happen
        | Some((p1, surround)) =>
          Some(OpSeqZ(skel, place_before(p1), surround))
        };
      | _ =>
        let k = OperatorSeq.surround_prefix_length(surround);
        let seq =
          OperatorSeq.opseq_of_exp_and_surround(erase(zp1), surround);
        Some(CursorP(OnDelim(k + 1, Before), OpSeq(skel, seq)));
      }
    }
  };
