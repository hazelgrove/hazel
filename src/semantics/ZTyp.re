[@deriving (show, sexp)]
type cursor_side = SemanticsCommon.cursor_side;

[@deriving (show, sexp)]
type opseq_surround = OperatorSeq.opseq_surround(UHTyp.t, UHTyp.op);
[@deriving (show, sexp)]
type opseq_prefix = OperatorSeq.opseq_prefix(UHTyp.t, UHTyp.op);
[@deriving (show, sexp)]
type opseq_suffix = OperatorSeq.opseq_suffix(UHTyp.t, UHTyp.op);

[@deriving (show, sexp)]
type t =
  | CursorT(cursor_side, UHTyp.t)
  | ParenthesizedZ(t)
  | ListZ(t)
  | OpSeqZ(UHTyp.skel_t, t, opseq_surround)
  /* in the pattern */
  | ForallZP(ZTPat.t, UHTyp.t)
  /* in the type */
  | ForallZT(TPat.t, t);

let place_before = (uty: UHTyp.t): t =>
  switch (uty) {
  | TVar(_)
  | Forall(_, _)
  | Hole
  | Parenthesized(_)
  | Unit
  | Num
  | Bool
  | List(_) => CursorT(Before, uty)
  | OpSeq(skel, seq) =>
    let (uty0, suffix) = OperatorSeq.split0(seq);
    let surround = OperatorSeq.EmptyPrefix(suffix);
    OpSeqZ(skel, CursorT(Before, uty0), surround);
  };

let place_after = (uty: UHTyp.t): t =>
  switch (uty) {
  | TVar(_)
  | Forall(_, _)
  | Hole
  | Parenthesized(_)
  | Unit
  | Num
  | Bool
  | List(_) => CursorT(After, uty)
  | OpSeq(skel, seq) =>
    let (uty0, prefix) = OperatorSeq.split_tail(seq);
    let surround = OperatorSeq.EmptySuffix(prefix);
    OpSeqZ(skel, CursorT(After, uty0), surround);
  };

let rec erase = (zty: t): UHTyp.t =>
  switch (zty) {
  | CursorT(_, ty) => ty
  | ParenthesizedZ(zty1) => Parenthesized(erase(zty1))
  | ListZ(zty1) => List(erase(zty1))
  | ForallZP(ztpat, uhtyp1) => Forall(ZTPat.erase(ztpat), uhtyp1)
  | ForallZT(tpat, zty1) => Forall(tpat, erase(zty1))
  | OpSeqZ(skel, zty1, surround) =>
    let uty1 = erase(zty1);
    OpSeq(skel, OperatorSeq.opseq_of_exp_and_surround(uty1, surround));
  };

let rec is_before = (zty: t): bool =>
  switch (zty) {
  | CursorT(Before, _) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | ForallZP(_) => false
  | ForallZT(_) => false
  | OpSeqZ(_, zty, EmptyPrefix(_)) => is_before(zty)
  | OpSeqZ(_, _, _) => false
  };

let rec is_after = (zty: t): bool =>
  switch (zty) {
  | CursorT(After, _) => true
  | CursorT(_, _) => false
  | ParenthesizedZ(_) => false
  | ListZ(_) => false
  | ForallZP(_) => false
  | ForallZT(_) => false
  | OpSeqZ(_, zty, EmptySuffix(_)) => is_after(zty)
  | OpSeqZ(_, _, _) => false
  };

let new_Forall = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u1, u_gen) = MetaVarGen.next(u_gen);
  (ForallZP(ZTPat.Cursor(Before, TPat.Hole(u1)), UHTyp.Hole), u_gen);
};
