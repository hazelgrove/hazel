open Sexplib.Std;
open GeneralUtil;

exception FreeVarInPat;

[@deriving sexp]
type op =
  | Comma
  | Space
  | Cons;

let is_Space =
  fun
  | Space => true
  | _ => false;

[@deriving sexp]
type skel_t = Skel.t(op);

[@deriving sexp]
type t =
  /* outer nodes */
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | Var(ErrStatus.t, VarErrStatus.t, VarWarnStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  /* inner nodes */
  | Parenthesized(t)
  | OpSeq(skel_t, opseq)
  | Inj(ErrStatus.t, InjSide.t, t)
and opseq = OperatorSeq.opseq(t, op);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      ~var_warn: VarWarnStatus.t=NoWarning,
      x: Var.t,
    )
    : t =>
  Var(err, var_err, var_warn, x);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool) => BoolLit(err, b);

let listnil = (~err: ErrStatus.t=NotInHole, ()): t => ListNil(err);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): ListMinTwo.t(skel_t) =>
  switch (skel2) {
  | BinOp(_, Comma, skel21, skel22) =>
    ListMinTwo.Cons(skel1, get_tuple(skel21, skel22))
  | BinOp(_, _, _, _)
  | Placeholder(_) => ListMinTwo.Pair(skel1, skel2)
  };

let rec make_tuple = (err: ErrStatus.t, skels: ListMinTwo.t(skel_t)) =>
  switch (skels) {
  | Pair(skel1, skel2) => Skel.BinOp(err, Comma, skel1, skel2)
  | Cons(skel1, skels) =>
    let skel2 = make_tuple(NotInHole, skels);
    Skel.BinOp(err, Comma, skel1, skel2);
  };

/* bidelimited patterns are those that don't have
 * sub-patterns at their outer left or right edge
 * in the concrete syntax */
let bidelimited = (p: t): bool =>
  switch (p) {
  /* outer nodes */
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => true
  /* inner nodes */
  | Inj(_, _, _) => true
  | Parenthesized(_) => true
  | OpSeq(_, _) => false
  };

/* if p is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = p =>
  if (bidelimited(p)) {
    p;
  } else {
    Parenthesized(p);
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (EmptyHole(u), u_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let rec get_err_status_t = (p: t): ErrStatus.t =>
  switch (p) {
  | EmptyHole(_) => NotInHole
  | Wild(err) => err
  | Var(err, _, _, _) => err
  | NumLit(err, _) => err
  | BoolLit(err, _) => err
  | ListNil(err) => err
  | Inj(err, _, _) => err
  | Parenthesized(p) => get_err_status_t(p)
  | OpSeq(BinOp(err, _, _, _), _) => err
  | OpSeq(Placeholder(n) as skel, seq) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(p_n) => get_err_status_t(p_n)
    }
  };

let rec set_err_status_t = (err: ErrStatus.t, p: t): t =>
  switch (p) {
  | EmptyHole(_) => p
  | Wild(_) => Wild(err)
  | Var(_, var_err, var_warn, x) => Var(err, var_err, var_warn, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Inj(_, inj_side, p) => Inj(err, inj_side, p)
  | Parenthesized(p) => Parenthesized(set_err_status_t(err, p))
  | OpSeq(skel, seq) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    OpSeq(skel, seq);
  }
and set_err_status_opseq =
    (err: ErrStatus.t, skel: skel_t, seq: opseq): (skel_t, opseq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(p_n) =>
      let p_n = set_err_status_t(err, p_n);
      switch (OperatorSeq.seq_update_nth(n, seq, p_n)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq)
      };
    }
  | BinOp(_, op, skel1, skel2) => (BinOp(err, op, skel1, skel2), seq)
  };

let is_inconsistent = (p: t): bool =>
  switch (get_err_status_t(p)) {
  | InHole(TypeInconsistent, _) => true
  | _ => false
  };

/* put p in a new hole, if it is not already in a hole */
let rec make_t_inconsistent = (u_gen: MetaVarGen.t, p: t): (t, MetaVarGen.t) =>
  switch (p) {
  /* already in hole */
  | EmptyHole(_)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) => (p, u_gen)
  /* not in hole */
  | Wild(NotInHole | InHole(WrongLength, _))
  | Var(NotInHole | InHole(WrongLength, _), _, _, _)
  | NumLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Inj(NotInHole | InHole(WrongLength, _), _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let p = set_err_status_t(InHole(TypeInconsistent, u), p);
    (p, u_gen);
  | Parenthesized(p) =>
    let (p, u_gen) = make_t_inconsistent(u_gen, p);
    (Parenthesized(p), u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, u_gen) = make_opseq_inconsistent(u_gen, skel, seq);
    (OpSeq(skel, seq), u_gen);
  }
and make_opseq_inconsistent =
    (u_gen: MetaVarGen.t, skel: skel_t, seq: opseq)
    : (skel_t, opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(p_n) =>
      let (p_n, u_gen) = make_t_inconsistent(u_gen, p_n);
      switch (OperatorSeq.seq_update_nth(n, seq, p_n)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, u_gen)
      };
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => (skel, seq, u_gen)
  | BinOp(NotInHole, _, _, _)
  | BinOp(InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let (skel, seq) =
      set_err_status_opseq(InHole(TypeInconsistent, u), skel, seq);
    (skel, seq, u_gen);
  };

let child_indices =
  fun
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => []
  | Parenthesized(_) => [0]
  | Inj(_, _, _) => [0]
  | OpSeq(_, seq) => range(OperatorSeq.seq_length(seq));

let favored_child: t => option((ChildIndex.t, t)) =
  fun
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | OpSeq(_, _) => None
  | Parenthesized(p)
  | Inj(_, _, p) => Some((0, p));
