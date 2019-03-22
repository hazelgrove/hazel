open SemanticsCommon;

type opseq_surround = OperatorSeq.opseq_surround(UHPat.t, UHPat.op);
type opseq_prefix = OperatorSeq.opseq_prefix(UHPat.t, UHPat.op);
type opseq_suffix = OperatorSeq.opseq_suffix(UHPat.t, UHPat.op);

type t =
  | CursorP(cursor_side, UHPat.t)
  | ParenthesizedZ(t)
  | Deeper(err_status, t')
and t' =
  | InjZ(inj_side, t)
  | OpSeqZ(UHPat.skel_t, t, opseq_surround);

let bidelimit = (zp: t): t =>
  switch (zp) {
  | CursorP(cursor_side, p) => CursorP(cursor_side, UHPat.bidelimit(p))
  | ParenthesizedZ(_)
  | Deeper(_, InjZ(_, _)) =>
    /* | Deeper _ (ListLitZ _) */
    zp
  | Deeper(_, OpSeqZ(_, _, _)) => ParenthesizedZ(zp)
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (hole, u_gen) = UHPat.new_EmptyHole(u_gen);
  (CursorP(Before, hole), u_gen);
};

let rec set_err_status = (err: err_status, zp: t): t =>
  switch (zp) {
  | CursorP(cursor_side, p) =>
    let p = UHPat.set_err_status(err, p);
    CursorP(cursor_side, p);
  | Deeper(_, OpSeqZ(Skel.BinOp(_, op, skel1, skel2), zp0, surround)) =>
    Deeper(err, OpSeqZ(Skel.BinOp(err, op, skel1, skel2), zp0, surround))
  | Deeper(_, ze') => Deeper(err, ze')
  | ParenthesizedZ(zp1) => ParenthesizedZ(set_err_status(err, zp1))
  };

let rec make_inconsistent = (u_gen: MetaVarGen.t, zp: t): (t, MetaVarGen.t) =>
  switch (zp) {
  | CursorP(cursor_side, p) =>
    let (p, u_gen) = UHPat.make_inconsistent(u_gen, p);
    (CursorP(cursor_side, p), u_gen);
  | Deeper(InHole(TypeInconsistent, _), _) => (zp, u_gen)
  | Deeper(NotInHole, zp')
  | Deeper(InHole(WrongLength, _), zp') =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (Deeper(InHole(TypeInconsistent, u), zp'), u_gen);
  | ParenthesizedZ(zp1) =>
    let (zp1, u_gen) = make_inconsistent(u_gen, zp1);
    (ParenthesizedZ(zp1), u_gen);
  };

let rec erase = (zp: t): UHPat.t =>
  switch (zp) {
  | CursorP(_, p) => p
  | Deeper(err_status, zp') => Pat(err_status, erase'(zp'))
  | ParenthesizedZ(zp) => Parenthesized(erase(zp))
  }
and erase' = (zp': t'): UHPat.t' =>
  switch (zp') {
  | InjZ(side, zp1) => Inj(side, erase(zp1))
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround));
  };

let is_after = (zp: t): bool =>
  switch (zp) {
  | CursorP(After, _) => true
  | Deeper(_, OpSeqZ(_, CursorP(After, _), EmptySuffix(_))) => true
  | CursorP(_, _)
  | Deeper(_, _)
  | ParenthesizedZ(_) => false
  };

let place_before = (p: UHPat.t): t =>
  switch (p) {
  | Parenthesized(_)
  | Pat(_, EmptyHole(_))
  | Pat(_, Wild)
  | Pat(_, Var(_, _))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil) => CursorP(Before, p)
  | Pat(err, OpSeq(skel, seq)) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    Deeper(err, OpSeqZ(skel, CursorP(Before, p0), surround));
  };

let place_after = (p: UHPat.t): t =>
  switch (p) {
  | Parenthesized(_)
  | Pat(_, EmptyHole(_))
  | Pat(_, Wild)
  | Pat(_, Var(_, _))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil) => CursorP(After, p)
  | Pat(err, OpSeq(skel, seq)) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    Deeper(err, OpSeqZ(skel, CursorP(After, p0), surround));
  };

let rec is_before = (zp: t): bool =>
  switch (zp) {
  | CursorP(Before, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, OpSeqZ(_, zp1, EmptyPrefix(_))) => is_before(zp1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  };

let rec is_after = (zp: t): bool =>
  switch (zp) {
  | CursorP(After, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, OpSeqZ(_, zp1, EmptySuffix(_))) => is_after(zp1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  };
