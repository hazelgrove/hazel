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
  | BoolLit(err_status, int)
  | ListNil(err_status)
and t_inner =
  | Inj(err_status, inj_side, t)
  | Parenthesized(t)
  | OpSeq(skel_t, opseq)
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
let bidelimited =
  fun
  | PO(_) => true
  | PI(Inj(_, _, _)) => true
  | PI(Parenthesized(_)) => true
  | PI(OpSeq(_, _)) => false;

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
  | PO(EmptyHole(_)) => p
  | PO(Wild(_)) => PO(Wild(err))
  | PO(Var(_, var_err, x)) => PO(Var(err, var_err, x))
  | PO(NumLit(_, n)) => PO(NumLit(err, n))
  | PO(BoolLit(_, b)) => PO(BoolLit(err, b))
  | PO(ListNil(_)) => PO(ListNil(err))
  | PI(Inj(_, inj_side, p)) => PI(Inj(err, inj_side, p))
  | PI(Parenthesized(p)) => PI(Parenthesized(set_err_status_t(err, p)))
  | PI(OpSeq(skel, seq)) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    PI(OpSeq(skel, seq));
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
  /* already in hole */
  | PO(EmptyHole(_))
  | PO(Wild(InHole(TypeInconsistent, _)))
  | PO(Var(InHole(TypeInconsistent, _), _, _))
  | PO(NumLit(InHole(TypeInconsistent, _), _))
  | PO(BoolLit(InHole(TypeInconsistent, _), _))
  | PO(ListNil(InHole(TypeInconsistent, _)))
  | PI(Inj(InHole(TypeInconsistent, _), _, _)) => (p, u_gen)
  /* not in hole */
  | PO(Wild(NotInHole))
  | PO(Wild(InHole(WrongLength, _)))
  | PO(Var(NotInHole, _, _))
  | PO(Var(InHole(WrongLength, _), _, _))
  | PO(NumLit(NotInHole, _))
  | PO(NumLit(InHole(WrongLength, _), _))
  | PO(BoolLit(NotInHole, _))
  | PO(BoolLit(InHole(WrongLength, _), _))
  | PO(ListNil(NotInHole))
  | PO(ListNil(InHole(WrongLength, _)))
  | PI(Inj(NotInHole, _, _))
  | PI(Inj(InHole(WrongLength, _), _, _)) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let p = set_err_status_t(InHole(TypeInconsistent, u), p);
    (p, u_gen);
  | PI(Parenthesized(p1)) =>
    let (p1, u_gen) = make_t_inconsistent(u_gen, p1);
    (PI(Parenthesized(p1)), u_gen);
  | PI(OpSeq(skel, seq)) =>
    let (skel, seq, u_gen) = make_opseq_inconsistent(u_gen, skel, seq);
    (PI(OpSeq(skel, seq)), u_gen);
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
