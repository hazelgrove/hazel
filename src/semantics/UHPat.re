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
  | Parenthesized(t)
  | EmptyHole(MetaVar.t)
  | OpSeq(skel_t, opseq)
  | Pat(err_status, t')
and t' =
  | Wild
  | Var(var_err_status, Var.t)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(inj_side, t)
  | ListNil
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
  | EmptyHole(_)
  | Pat(_, Wild)
  | Pat(_, Var(_, _))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil)
  | Parenthesized(_) => true
  | OpSeq(_, _) => false;

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

let rec get_err_status_t = (p: t): err_status =>
  switch (p) {
  | Pat(err_status, _) => err_status
  | Parenthesized(p1) => get_err_status_t(p1)
  | EmptyHole(_) => NotInHole
  | OpSeq(BinOp(err_status, _, _, _), _) => err_status
  | OpSeq(Placeholder(n) as skel, seq) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(p_n) => get_err_status_t(p_n)
    }
  };

let rec set_err_status_t = (err: err_status, p: t): t =>
  switch (p) {
  | Parenthesized(p1) => Parenthesized(set_err_status_t(err, p1))
  | EmptyHole(_) => p
  | Pat(_, p') => Pat(err, p')
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
  | Parenthesized(p1) =>
    let (p1, u_gen) = make_t_inconsistent(u_gen, p1);
    (Parenthesized(p1), u_gen);
  | EmptyHole(_) => (p, u_gen)
  | Pat(InHole(TypeInconsistent, _), _) => (p, u_gen)
  | Pat(NotInHole, _)
  | Pat(InHole(WrongLength, _), _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let p = set_err_status_t(InHole(TypeInconsistent, u), p);
    (p, u_gen);
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
