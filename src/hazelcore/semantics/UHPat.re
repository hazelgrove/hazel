open Sexplib.Std;

exception FreeVarInPat;

[@deriving sexp]
type operator =
  | Comma
  | Space
  | Cons;

let is_Space =
  fun
  | Space => true
  | _ => false;

let is_Comma =
  fun
  | Comma => true
  | _ => false;

[@deriving sexp]
type t =
  | P1(opseq)
  | P0(operand)
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Parenthesized(t)
  | Inj(ErrStatus.t, InjSide.t, t);

type skel = OpSeq.skel(operator);
type seq = OpSeq.seq(operand, operator);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let wild = (~err: ErrStatus.t=NotInHole, ()) => Wild(err);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool) => BoolLit(err, b);

let numlit = (~err: ErrStatus.t=NotInHole, n: int) => NumLit(err, n);

let listnil = (~err: ErrStatus.t=NotInHole, ()) => ListNil(err);

let rec get_tuple_elements: skel => list(skel) =
  fun
  | BinOp(_, Comma, skel1, skel2) =>
    get_tuple_elements(skel1) @ get_tuple_elements(skel2)
  | skel => [skel];

let rec make_tuple = (err: ErrStatus.t, elements: list(skel)): skel =>
  switch (elements) {
  | [] => failwith("make_tuple: expected at least 1 element")
  | [skel] => skel
  | [skel, ...skels] =>
    BinOp(err, Comma, skel, make_tuple(NotInHole, skels))
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
    Parenthesized(P0(p));
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

let rec get_err_status: t => ErrStatus.t =
  fun
  | P1(opseq) => opseq |> get_err_status_opseq
  | P0(operand) => operand |> get_err_status_operand
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
  switch (p) {
  | P1(opseq) => P1(opseq |> set_err_status_opseq(err))
  | P0(operand) => P0(operand |> set_err_status_operand(err))
  }
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
  switch (p) {
  | P1(opseq) =>
    let (opseq, u_gen) = opseq |> make_inconsistent_opseq(u_gen);
    (P1(opseq), u_gen);
  | P0(operand) =>
    let (operand, u_gen) = operand |> make_inconsistent_operand(u_gen);
    (P0(operand), u_gen);
  }
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

let favored_child: operand => option((ChildIndex.t, t)) =
  fun
  | EmptyHole(_)
  | Wild(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => None
  | Parenthesized(p)
  | Inj(_, _, p) => Some((0, p));
