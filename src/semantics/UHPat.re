open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

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
  | PO(t_outer)
  | PI(t_inner)
and t_outer =
  | EmptyHole(MetaVar.t)
  | Wild(err_status)
  | Var(err_status, var_err_status, Var.t)
  | NumLit(err_status, int)
  | BoolLit(err_status, bool)
  | ListNil(err_status)
and t_inner =
  | Parenthesized(t)
  | OpSeq(skel_t, opseq)
  | Inj(err_status, inj_side, t)
and opseq = OperatorSeq.opseq(t, op);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): ListMinTwo.t(skel_t) =>
  switch (skel2) {
  | BinOp(_, Comma, skel21, skel22) =>
    ListMinTwo.Cons(skel1, get_tuple(skel21, skel22))
  | BinOp(_, _, _, _)
  | Placeholder(_) => ListMinTwo.Pair(skel1, skel2)
  };

let rec make_tuple = (err: err_status, skels: ListMinTwo.t(skel_t)) =>
  switch (skels) {
  | Pair(skel1, skel2) => Skel.BinOp(err, Comma, skel1, skel2)
  | Cons(skel1, skels) =>
    let skel2 = make_tuple(NotInHole, skels);
    Skel.BinOp(err, Comma, skel1, skel2);
  };

/* bidelimited patterns are those that don't have
 * sub-patterns at their outer left or right edge
 * in the concrete syntax */
let bidelimited_outer = (_: t_outer): bool => true;
let bidelimited_inner = (pi: t_inner): bool =>
  switch (pi) {
  | Inj(_, _, _) => true
  | Parenthesized(_) => true
  | OpSeq(_, _) => false
  };
let bidelimited = (p: t): bool =>
  switch (p) {
  | PO(p_outer) => bidelimited_outer(p_outer)
  | PI(p_inner) => bidelimited_inner(p_inner)
  };

/* if p is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = p =>
  if (bidelimited(p)) {
    p;
  } else {
    PI(Parenthesized(p));
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (PO(EmptyHole(u)), u_gen);
};

let is_EmptyHole =
  fun
  | PO(EmptyHole(_)) => true
  | _ => false;

let rec get_err_status_t = (p: t): err_status =>
  switch (p) {
  | PO(EmptyHole(_)) => NotInHole
  | PO(Wild(err)) => err
  | PO(Var(err, _, _)) => err
  | PO(NumLit(err, _)) => err
  | PO(BoolLit(err, _)) => err
  | PO(ListNil(err)) => err
  | PI(Inj(err, _, _)) => err
  | PI(Parenthesized(p)) => get_err_status_t(p)
  | PI(OpSeq(BinOp(err, _, _, _), _)) => err
  | PI(OpSeq(Placeholder(n) as skel, seq)) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(p_n) => get_err_status_t(p_n)
    }
  };

let rec set_err_status_t = (err: err_status, p: t): t =>
  switch (p) {
  | PO(po) => PO(set_err_status_t_outer(err, po))
  | PI(pi) => PI(set_err_status_t_inner(err, pi))
  }
and set_err_status_t_outer = (err: err_status, po: t_outer): t_outer =>
  switch (po) {
  | EmptyHole(_) => po
  | Wild(_) => Wild(err)
  | Var(_, var_err, x) => Var(err, var_err, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  }
and set_err_status_t_inner = (err: err_status, pi: t_inner): t_inner =>
  switch (pi) {
  | Inj(_, inj_side, p) => Inj(err, inj_side, p)
  | Parenthesized(p) => Parenthesized(set_err_status_t(err, p))
  | OpSeq(skel, seq) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    OpSeq(skel, seq);
  }
and set_err_status_opseq =
    (err: err_status, skel: skel_t, seq: opseq): (skel_t, opseq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
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

/* put p in a new hole, if it is not already in a hole */
let rec make_t_inconsistent = (u_gen: MetaVarGen.t, p: t): (t, MetaVarGen.t) =>
  switch (p) {
  | PO(po) =>
    let (po, u_gen) = make_t_outer_inconsistent(u_gen, po);
    (PO(po), u_gen);
  | PI(pi) =>
    let (pi, u_gen) = make_t_inner_inconsistent(u_gen, pi);
    (PI(pi), u_gen);
  }
and make_t_outer_inconsistent =
    (u_gen: MetaVarGen.t, po: t_outer): (t_outer, MetaVarGen.t) =>
  switch (po) {
  /* already in hole */
  | EmptyHole(_)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _)) => (po, u_gen)
  /* not in hole */
  | Wild(NotInHole)
  | Wild(InHole(WrongLength, _))
  | Var(NotInHole, _, _)
  | Var(InHole(WrongLength, _), _, _)
  | NumLit(NotInHole, _)
  | NumLit(InHole(WrongLength, _), _)
  | BoolLit(NotInHole, _)
  | BoolLit(InHole(WrongLength, _), _)
  | ListNil(NotInHole)
  | ListNil(InHole(WrongLength, _)) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let po = set_err_status_t_outer(InHole(TypeInconsistent, u), po);
    (po, u_gen);
  }
and make_t_inner_inconsistent =
    (u_gen: MetaVarGen.t, pi: t_inner): (t_inner, MetaVarGen.t) =>
  switch (pi) {
  /* already in hole */
  | Inj(InHole(TypeInconsistent, _), _, _) => (pi, u_gen)
  /* not in hole */
  | Inj(NotInHole, _, _)
  | Inj(InHole(WrongLength, _), _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let pi = set_err_status_t_inner(InHole(TypeInconsistent, u), pi);
    (pi, u_gen);
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
    switch (OperatorSeq.seq_nth(n, seq)) {
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
