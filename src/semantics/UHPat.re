open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

[@deriving sexp]
type operator =
  | Comma
  | Space
  | Cons;

let is_Space =
  fun
  | Space => true
  | _ => false;

[@deriving sexp]
type t = opseq
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Parenthesized(t)
  | Inj(ErrStatus.t, inj_side, t);

type skel = OpSeq.skel(operator);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool) => BoolLit(err, b);

let listnil = (~err: ErrStatus.t=NotInHole, ()) => ListNil(err);

let rec get_tuple = (skel1: skel, skel2: skel): ListMinTwo.t(skel) =>
  switch (skel2) {
  | BinOp(_, Comma, skel21, skel22) =>
    ListMinTwo.Cons(skel1, get_tuple(skel21, skel22))
  | BinOp(_, _, _, _)
  | Placeholder(_) => ListMinTwo.Pair(skel1, skel2)
  };

let rec make_tuple = (err: ErrStatus.t, skels: ListMinTwo.t(skel)) =>
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
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | Inj(_, _, _)
  | Parenthesized(_) => true;

/* if p is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = p =>
  if (bidelimited(p)) {
    p;
  } else {
    Parenthesized(OpSeq.wrap(p));
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (operand, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (EmptyHole(u), u_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let rec get_err_status = (p: t): ErrStatus.t => get_err_status_opseq(p)
and get_err_status_opseq = opseq =>
  OpSeq.get_err_status(~get_err_status_operand, opseq)
and get_err_status_operand =
  fun
  | EmptyHole(_) => NotInHole
  | Wild(err)
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | Inj(err, _, _) => err
  | Parenthesized(p) => get_err_status(p);

let rec set_err_status = (err: ErrStatus.t, p: t): t =>
  set_err_status_opseq(err, p)
and set_err_status_opseq = (err, opseq) =>
  OpSeq.set_err_status(~set_err_status_operand, err, opseq)
and set_err_status_operand = (err, operand) =>
  switch (operand) {
  | EmptyHole(_) => operand
  | Wild(_) => Wild(err)
  | Var(_, var_err, x) => Var(err, var_err, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Inj(_, inj_side, p) => Inj(err, inj_side, p)
  | Parenthesized(p) => Parenthesized(set_err_status(err, p))
  };

let is_inconsistent = (p: t): bool =>
  switch (get_err_status(p)) {
  | InHole(TypeInconsistent, _) => true
  | _ => false
  };

/* put p in a new hole, if it is not already in a hole */
let rec make_inconsistent = (u_gen: MetaVarGen.t, p: t): (t, MetaVarGen.t) =>
  make_inconsistent_opseq(u_gen, p)
and make_inconsistent_opseq =
    (u_gen: MetaVarGen.t, opseq: opseq): (opseq, MetaVarGen.t) =>
  opseq |> OpSeq.make_inconsistent(~make_inconsistent_operand, u_gen)
and make_inconsistent_operand =
    (u_gen: MetaVarGen.t, operand: operand): (operand, MetaVarGen.t) =>
  switch (operand) {
  // already in hole
  | EmptyHole(_)
  | Wild(InHole(TypeInconsistent, _))
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Inj(InHole(TypeInconsistent, _), _, _) => (operand, u_gen)
  // not in hole
  | Wild(NotInHole | InHole(WrongLength, _))
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | NumLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Inj(NotInHole | InHole(WrongLength, _), _, _) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let set_operand =
      operand |> set_err_status_operand(InHole(TypeInconsistent, u));
    (set_operand, u_gen);
  | Parenthesized(p) =>
    let (set_p, u_gen) = p |> make_inconsistent(u_gen);
    (Parenthesized(set_p), u_gen);
  };

let child_indices_operand =
  fun
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => []
  | Parenthesized(_) => [0]
  | Inj(_, _, _) => [0];
let child_indices_opseq = OpSeq.child_indices;
let child_indices = child_indices_opseq;

let favored_child: operand => option((child_index, t)) =
  fun
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => None
  | Parenthesized(p)
  | Inj(_, _, p) => Some((0, p));
