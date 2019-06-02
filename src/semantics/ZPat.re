open SemanticsCommon;
open GeneralUtil;

[@deriving sexp]
type opseq_surround = OperatorSeq.opseq_surround(UHPat.t, UHPat.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHPat.t, UHPat.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHPat.t, UHPat.op);

[@deriving sexp]
type t =
  | CursorP(cursor_pos, UHPat.t)
  /* zipper cases */
  | ParenthesizedZ(t)
  | OpSeqZ(UHPat.skel_t, t, opseq_surround)
  | InjZ(err_status, inj_side, t);

exception SkelInconsistentWithOpSeq;

let valid_cursors = (p: UHPat.t): list(cursor_pos) =>
  switch (p) {
  /* outer nodes */
  | EmptyHole(_) => outer_cursors(1)
  | Wild(_) => outer_cursors(1)
  | Var(_, _, x) => outer_cursors(Var.length(x))
  | NumLit(_, n) => outer_cursors(num_digits(n))
  | BoolLit(_, b) => outer_cursors(b ? 4 : 5)
  | ListNil(_) => outer_cursors(2)
  /* inner nodes */
  | Inj(_, _, _) => inner_cursors(2)
  | Parenthesized(_) => inner_cursors(2)
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.map(k => k + 1)
    |> List.map(k => inner_cursors_k(k))
    |> List.flatten
  };

let is_valid_cursor = (cursor: cursor_pos, p: UHPat.t): bool =>
  contains(valid_cursors(p), cursor);

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

let rec set_err_status_t = (err: err_status, zp: t): t =>
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
    (err: err_status, skel: UHPat.skel_t, zp_n: t, surround: opseq_surround)
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
  | InjZ(NotInHole | InHole(WrongLength, _), inj_side, zp1) =>
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
  | BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
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
  | InjZ(err_status, inj_side, zp1) => Inj(err_status, inj_side, erase(zp1))
  | ParenthesizedZ(zp) => Parenthesized(erase(zp))
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround));
  };

let rec is_before = (zp: t): bool =>
  switch (zp) {
  /* outer nodes */
  | CursorP(cursor, EmptyHole(_))
  | CursorP(cursor, Wild(_))
  | CursorP(cursor, Var(_, _, _))
  | CursorP(cursor, NumLit(_, _))
  | CursorP(cursor, BoolLit(_, _))
  | CursorP(cursor, ListNil(_)) => cursor == outer_cursor(0)
  /* inner nodes */
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == inner_cursor(0, Before)
  | CursorP(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptyPrefix(_)) => is_before(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zp: t): bool =>
  switch (zp) {
  /* outer nodes */
  | CursorP(cursor, EmptyHole(_)) => cursor == outer_cursor(1)
  | CursorP(cursor, Wild(_)) => cursor == outer_cursor(1)
  | CursorP(cursor, Var(_, _, x)) => cursor == outer_cursor(Var.length(x))
  | CursorP(cursor, NumLit(_, n)) => cursor == outer_cursor(num_digits(n))
  | CursorP(cursor, BoolLit(_, b)) => cursor == outer_cursor(b ? 4 : 5)
  | CursorP(cursor, ListNil(_)) => cursor == outer_cursor(2)
  /* inner nodes */
  | CursorP(cursor, Inj(_, _, _))
  | CursorP(cursor, Parenthesized(_)) => cursor == inner_cursor(1, After)
  | CursorP(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptySuffix(_)) => is_after(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes */
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => CursorP(outer_cursor(0), p)
  /* inner nodes */
  | Inj(_, _, _)
  | Parenthesized(_) => CursorP(inner_cursor(0, Before), p)
  | OpSeq(skel, seq) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zp0 = place_before(p0);
    OpSeqZ(skel, zp0, surround);
  };

let rec place_after = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes */
  | EmptyHole(_) => CursorP(outer_cursor(1), p)
  | Wild(_) => CursorP(outer_cursor(1), p)
  | Var(_, _, x) => CursorP(outer_cursor(Var.length(x)), p)
  | NumLit(_, n) => CursorP(outer_cursor(num_digits(n)), p)
  | BoolLit(_, b) => CursorP(outer_cursor(b ? 4 : 5), p)
  | ListNil(_) => CursorP(outer_cursor(2), p)
  /* inner nodes */
  | Inj(_, _, _) => CursorP(inner_cursor(1, After), p)
  | Parenthesized(_) => CursorP(inner_cursor(1, After), p)
  | OpSeq(skel, seq) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zp0 = place_after(p0);
    OpSeqZ(skel, zp0, surround);
  };

let place_cursor = (cursor: cursor_pos, p: UHPat.t): option(t) =>
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

let node_positions = (p: UHPat.t): list(node_pos) =>
  switch (p) {
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => node_positions(valid_cursors(p))
  | Parenthesized(_)
  | Inj(_) =>
    node_positions(inner_cursors_k(0))
    @ [Deeper(0)]
    @ node_positions(inner_cursors_k(1))
  | OpSeq(_, seq) =>
    range(OperatorSeq.seq_length(seq))
    |> List.fold_left(
         (lstSoFar, i) =>
           switch (lstSoFar) {
           | [] => [Deeper(i)]
           | [_, ..._] =>
             lstSoFar @ node_positions(inner_cursors_k(i)) @ [Deeper(i)]
           },
         [],
       )
  };

let node_position_of_t = (zp: t): node_pos =>
  switch (zp) {
  | CursorP(cursor, _) => On(cursor)
  | ParenthesizedZ(_) => Deeper(0)
  | InjZ(_, _, _) => Deeper(0)
  | OpSeqZ(_, _, surround) =>
    Deeper(OperatorSeq.surround_prefix_length(surround))
  };
