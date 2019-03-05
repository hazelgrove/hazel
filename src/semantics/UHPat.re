open SemanticsCommon;
open HazelUtil;

type op =
  | Comma
  | Space
  | Cons;

let is_Space =
  fun
  | Space => true
  | _ => false;

type skel_t = Skel.t(op);

type t =
  | Pat(err_status, t')
  | Parenthesized(t)
and t' =
  | EmptyHole(MetaVar.t)
  | Wild
  | Var(Var.t)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(inj_side, t)
  | ListNil
  /* | ListLit : list(t) -> t' */
  | OpSeq(skel_t, OperatorSeq.opseq(t, op));

type opseq = OperatorSeq.opseq(t, op);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): TupleList.t(skel_t) =>
  switch (skel2) {
  | Skel.BinOp(_, Comma, skel21, skel22) =>
    TupleList.Cons(skel1, get_tuple(skel21, skel22))
  | Skel.BinOp(_, _, _, _)
  | Skel.Placeholder(_) => TupleList.Pair(skel1, skel2)
  };

let rec make_tuple = (err: err_status, skels: TupleList.t(skel_t)) =>
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
  | Pat(_, EmptyHole(_))
  | Pat(_, Wild)
  | Pat(_, Var(_))
  | Pat(_, NumLit(_))
  | Pat(_, BoolLit(_))
  | Pat(_, Inj(_, _))
  | Pat(_, ListNil)
  /* | Pat _ (ListLit _) */
  | Parenthesized(_) => true
  | Pat(_, OpSeq(_, _)) => false;

/* if p is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = p =>
  if (bidelimited(p)) {
    p;
  } else {
    Parenthesized(p);
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = u_gen => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (Pat(NotInHole, EmptyHole(u)), u_gen);
};

let is_EmptyHole =
  fun
  | Pat(_, EmptyHole(_)) => true
  | _ => false;

let rec set_err_status = err =>
  fun
  | Pat(_, OpSeq(Skel.BinOp(_, op, skel1, skel2), seq)) =>
    Pat(err, OpSeq(Skel.BinOp(err, op, skel1, skel2), seq))
  | Pat(_, p') => Pat(err, p')
  | Parenthesized(p') => Parenthesized(set_err_status(err, p'));

/* put p in a new hole, if it is not already in a hole */
let rec make_inconsistent = (u_gen, p) =>
  switch (p) {
  | Pat(NotInHole, _)
  | Pat(InHole(WrongLength, _), _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let p = set_err_status(InHole(TypeInconsistent, u), p);
    (p, u_gen);
  | Pat(InHole(TypeInconsistent, _), _) => (p, u_gen)
  | Parenthesized(p1) =>
    switch (make_inconsistent(u_gen, p1)) {
    | (p1, u_gen) => (Parenthesized(p1), u_gen)
    }
  };
