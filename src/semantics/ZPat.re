open GeneralUtil;
open SemanticsCommon;

type opseq_surround = OperatorSeq.opseq_surround(UHPat.t, UHPat.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHPat.t, UHPat.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHPat.t, UHPat.op);

type t =
  | CursorPO(outer_cursor, UHPat.t_outer)
  | CursorPI(inner_cursor, UHPat.t_inner)
  /* zipper cases */
  | ParenthesizedZ(t)
  | OpSeqZ(UHPat.skel_t, t, opseq_surround)
  | InjZ(err_status, inj_side, t);

exception SkelInconsistentWithOpSeq;

let bidelimit = (zp: t): t =>
  switch (zp) {
  | CursorPO(_, po) =>
    if (UHPat.bidelimited_outer(po)) {
      zp;
    } else {
      ParenthesizedZ(zp);
    }
  | CursorPI(_, pi) =>
    if (UHPat.bidelimited_inner(pi)) {
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
  | CursorPO(outer_cursor, po) =>
    let po = UHPat.set_err_status_t_outer(err, po);
    CursorPO(outer_cursor, po);
  | CursorPI(outer_cursor, pi) =>
    let pi = UHPat.set_err_status_t_inner(err, pi);
    CursorPI(outer_cursor, pi);
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
  | CursorPO(outer_cursor, po) =>
    let (po, u_gen) = UHPat.make_t_outer_inconsistent(u_gen, po);
    (CursorPO(outer_cursor, po), u_gen);
  | CursorPI(inner_cursor, pi) =>
    let (pi, u_gen) = UHPat.make_t_inner_inconsistent(u_gen, pi);
    (CursorPI(inner_cursor, pi), u_gen);
  | InjZ(InHole(TypeInconsistent, _), _, _) => (zp, u_gen)
  | InjZ(NotInHole, inj_side, zp1)
  | InjZ(InHole(WrongLength, _), inj_side, zp1) =>
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
  | CursorPO(_, po) => PO(po)
  | CursorPI(_, pi) => PI(pi)
  | InjZ(err_status, inj_side, zp1) =>
    PI(Inj(err_status, inj_side, erase(zp1)))
  | ParenthesizedZ(zp) => PI(Parenthesized(erase(zp)))
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    PI(OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround)));
  };

let rec is_before = (zp: t): bool =>
  switch (zp) {
  /* outer nodes */
  | CursorPO(Char(j), EmptyHole(_))
  | CursorPO(Char(j), Wild(_))
  | CursorPO(Char(j), Var(_, _, _))
  | CursorPO(Char(j), NumLit(_, _))
  | CursorPO(Char(j), BoolLit(_, _))
  | CursorPO(Char(j), ListNil(_)) => j === 0
  /* inner nodes */
  | CursorPI(inner_cursor, Inj(_, _, _))
  | CursorPI(inner_cursor, Parenthesized(_)) =>
    inner_cursor === BeforeChild(0, Before)
  | CursorPI(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptyPrefix(_)) => is_before(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zp: t): bool =>
  switch (zp) {
  /* outer nodes */
  | CursorPO(Char(j), EmptyHole(_)) => j === 1
  | CursorPO(Char(j), Wild(_)) => j === 1
  | CursorPO(Char(j), ListNil(_)) => j === 2
  | CursorPO(Char(j), Var(_, _, x)) => j === Var.length(x)
  | CursorPO(Char(j), NumLit(_, n)) => j === num_digits(n)
  | CursorPO(Char(j), BoolLit(_, b)) => j === 4 && b || j === 5 && !b
  /* inner nodes */
  | CursorPI(inner_cursor, Inj(_, _, _))
  | CursorPI(inner_cursor, Parenthesized(_)) =>
    inner_cursor === ClosingDelimiter(After)
  | CursorPI(_, OpSeq(_, _)) => false
  /* zipper cases */
  | InjZ(_, _, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptySuffix(_)) => is_after(zp1)
  | OpSeqZ(_, _, _) => false
  };

let rec place_before = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes */
  | PO(EmptyHole(_) as po)
  | PO(Wild(_) as po)
  | PO(Var(_, _, _) as po)
  | PO(NumLit(_, _) as po)
  | PO(BoolLit(_, _) as po)
  | PO(ListNil(_) as po) => CursorPO(Char(0), po)
  /* inner nodes */
  | PI(Inj(_, _, _) as pi)
  | PI(Parenthesized(_) as pi) => CursorPI(BeforeChild(0, Before), pi)
  | PI(OpSeq(skel, seq)) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    let zp0 = place_before(p0);
    OpSeqZ(skel, zp0, surround);
  };

let rec place_after = (p: UHPat.t): t =>
  switch (p) {
  /* outer nodes */
  | PO(EmptyHole(_) as po) => CursorPO(Char(1), po)
  | PO(Wild(_) as po) => CursorPO(Char(1), po)
  | PO(Var(_, _, x) as po) => CursorPO(Char(Var.length(x)), po)
  | PO(NumLit(_, n) as po) => CursorPO(Char(num_digits(n)), po)
  | PO(BoolLit(_, true) as po) => CursorPO(Char(4), po)
  | PO(BoolLit(_, false) as po) => CursorPO(Char(5), po)
  | PO(ListNil(_) as po) => CursorPO(Char(2), po)
  /* inner nodes */
  | PI(Inj(_, _, _) as pi)
  | PI(Parenthesized(_) as pi) => CursorPI(ClosingDelimiter(After), pi)
  | PI(OpSeq(skel, seq)) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    let zp0 = place_after(p0);
    OpSeqZ(skel, zp0, surround);
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (hole, u_gen) = UHPat.new_EmptyHole(u_gen);
  (place_before(hole), u_gen);
};
