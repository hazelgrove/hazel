open SemanticsCommon;

type opseq_surround = OperatorSeq.opseq_surround(UHPat.t, UHPat.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHPat.t, UHPat.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHPat.t, UHPat.op);

type t =
  | CursorP(cursor_pos, UHPat.t)
  | ParenthesizedZ(t)
  | OpSeqZ(UHPat.skel_t, t, opseq_surround)
  | Deeper(err_status, t')
and t' =
  | InjZ(inj_side, t);

exception SkelInconsistentWithOpSeq;

let bidelimit = (zp: t): t =>
  switch (zp) {
  | CursorP(cursor_pos, p) => CursorP(cursor_pos, UHPat.bidelimit(p))
  | ParenthesizedZ(_)
  | Deeper(_, InjZ(_, _)) => zp
  | OpSeqZ(_, _, _) => ParenthesizedZ(zp)
  };

let rec set_err_status_t = (err: err_status, zp: t): t =>
  switch (zp) {
  | CursorP(cursor_pos, p) =>
    let p = UHPat.set_err_status_t(err, p);
    CursorP(cursor_pos, p);
  | ParenthesizedZ(zp1) => ParenthesizedZ(set_err_status_t(err, zp1))
  | Deeper(_, ze') => Deeper(err, ze')
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
  | CursorP(cursor_pos, p) =>
    let (p, u_gen) = UHPat.make_t_inconsistent(u_gen, p);
    (CursorP(cursor_pos, p), u_gen);
  | Deeper(InHole(TypeInconsistent, _), _) => (zp, u_gen)
  | Deeper(NotInHole, zp')
  | Deeper(InHole(WrongLength, _), zp') =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (Deeper(InHole(TypeInconsistent, u), zp'), u_gen);
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
  | Deeper(err_status, zp') => Pat(err_status, erase'(zp'))
  | ParenthesizedZ(zp) => Parenthesized(erase(zp))
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround));
  }
and erase' = (zp': t'): UHPat.t' =>
  switch (zp') {
  | InjZ(side, zp1) => Inj(side, erase(zp1))
  };

let rec is_before = (zp: t): bool =>
  switch (zp) {
  | CursorP(Before, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptyPrefix(_)) => is_before(zp1)
  | OpSeqZ(_, _, _) => false
  | Deeper(_, InjZ(_, _)) => false
  };

let rec is_after = (zp: t): bool =>
  switch (zp) {
  | CursorP(After, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | OpSeqZ(_, zp1, EmptySuffix(_)) => is_after(zp1)
  | OpSeqZ(_, _, _) => false
  | Deeper(_, InjZ(_, _)) => false
  };

let place_before = (p: UHPat.t): t =>
  switch (p) {
  | Parenthesized(_)
  | EmptyHole(_)
  | Pat(_, Wild)
  | Pat(_, Var(_, _))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil) => CursorP(Before, p)
  | OpSeq(skel, seq) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, CursorP(Before, p0), surround);
  };

let place_after = (p: UHPat.t): t =>
  switch (p) {
  | Parenthesized(_)
  | EmptyHole(_)
  | Pat(_, Wild)
  | Pat(_, Var(_, _))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil) => CursorP(After, p)
  | OpSeq(skel, seq) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, CursorP(After, p0), surround);
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (hole, u_gen) = UHPat.new_EmptyHole(u_gen);
  (CursorP(Before, hole), u_gen);
};
