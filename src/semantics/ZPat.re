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

let bidelimit = zp =>
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

let rec set_err_status = (err, zp) =>
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
  | Deeper(err_status, zp') => UHPat.Pat(err_status, erase'(zp'))
  | ParenthesizedZ(zp) => UHPat.Parenthesized(erase(zp))
  }
and erase' = (zp': t'): UHPat.t' =>
  switch (zp') {
  | InjZ(side, zp1) => UHPat.Inj(side, erase(zp1))
  /* | ListLitZ zps -> UHPat.ListLit (ZList.erase zps erase) */
  | OpSeqZ(skel, zp1, surround) =>
    let p1 = erase(zp1);
    UHPat.OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(p1, surround));
  };

let place_Before = (p: UHPat.t): t =>
  switch (p) {
  | UHPat.Parenthesized(_)
  | UHPat.Pat(_, UHPat.EmptyHole(_))
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, UHPat.Var(_))
  | UHPat.Pat(_, UHPat.NumLit(_))
  | UHPat.Pat(_, UHPat.BoolLit(_))
  | UHPat.Pat(_, UHPat.Inj(_, _))
  /* | UHPat.Pat _ (UHPat.ListLit _) -> */
  | UHPat.Pat(_, UHPat.ListNil) => CursorP(Before, p)
  | UHPat.Pat(err, UHPat.OpSeq(skel, seq)) =>
    let (p0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    Deeper(err, OpSeqZ(skel, CursorP(Before, p0), surround));
  };

let place_After = (p: UHPat.t): t =>
  switch (p) {
  | UHPat.Parenthesized(_)
  | UHPat.Pat(_, UHPat.EmptyHole(_))
  | UHPat.Pat(_, UHPat.Wild)
  | UHPat.Pat(_, UHPat.Var(_))
  | UHPat.Pat(_, UHPat.NumLit(_))
  | UHPat.Pat(_, UHPat.BoolLit(_))
  | UHPat.Pat(_, UHPat.Inj(_, _))
  /* | UHPat.Pat _ (UHPat.ListLit _) */
  | UHPat.Pat(_, UHPat.ListNil) => CursorP(After, p)
  | UHPat.Pat(err, UHPat.OpSeq(skel, seq)) =>
    let (p0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    Deeper(err, OpSeqZ(skel, CursorP(After, p0), surround));
  };

let rec cursor_at_start = (zp: t): bool =>
  switch (zp) {
  | CursorP(Before, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, OpSeqZ(_, zp1, EmptySuffix(_))) => cursor_at_start(zp1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  };

let rec cursor_at_end = (zp: t): bool =>
  switch (zp) {
  | CursorP(After, _) => true
  | CursorP(_, _) => false
  | ParenthesizedZ(_) => false
  | Deeper(_, OpSeqZ(_, zp1, EmptySuffix(_))) => cursor_at_end(zp1)
  | Deeper(_, OpSeqZ(_, _, _)) => false
  | Deeper(_, InjZ(_, _)) => false
  };