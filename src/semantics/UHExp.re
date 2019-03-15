open Sexplib.Std;
open SemanticsCommon;
open HazelUtil;

[@deriving sexp]
type op =
  | Plus
  | Times
  | LessThan
  | Space
  | Comma
  | Cons;

let is_Space =
  fun
  | Space => true
  | _ => false;

[@deriving sexp]
type skel_t = Skel.t(op);

[@deriving sexp]
type t =
  | Tm(err_status, t')
  | Parenthesized(t)
and t' =
  | Var(var_err_status, Var.t)
  | LineItem(line_item, t)
  | Lam(UHPat.t, option(UHTyp.t), t)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(inj_side, t)
  | Case(t, list(rule), option(UHTyp.t))
  | ListNil
  | EmptyHole(MetaVar.t)
  | OpSeq(skel_t, opseq) /* invariant: skeleton is consistent with opseq */
  | ApPalette(PaletteName.t, SerializedModel.t, SpliceInfo.t(t))
and line_item =
  | EmptyLine
  | ExpLine(t)
  | LetLine(UHPat.t, option(UHTyp.t), t)
and rule =
  | Rule(UHPat.t, t)
and rules = list(rule)
and opseq = OperatorSeq.opseq(t, op)
and splice_info = SpliceInfo.t(t)
and splice_map = SpliceInfo.splice_map(t);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): TupleList.t(skel_t) =>
  switch (skel2) {
  | Skel.BinOp(_, Comma, skel21, skel22) =>
    TupleList.Cons(skel1, get_tuple(skel21, skel22))
  | Skel.BinOp(_, _, _, _)
  | Skel.Placeholder(_) => TupleList.Pair(skel1, skel2)
  };

let rec make_tuple = (err: err_status, skels: TupleList.t(skel_t)): skel_t =>
  switch (skels) {
  | Pair(skel1, skel2) => Skel.BinOp(err, Comma, skel1, skel2)
  | Cons(skel1, skels) =>
    Skel.BinOp(err, Comma, skel1, make_tuple(NotInHole, skels))
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = u_gen => {
  let (u', u_gen') = MetaVarGen.next(u_gen);
  (Tm(NotInHole, EmptyHole(u')), u_gen');
};

let is_EmptyHole =
  fun
  | Tm(_, EmptyHole(_)) => true
  | _ => false;

let empty_rule = u_gen => {
  let (rule_p, u_gen) = UHPat.new_EmptyHole(u_gen);
  let (rule_e, u_gen) = new_EmptyHole(u_gen);
  let rule = Rule(rule_p, rule_e);
  (rule, u_gen);
};

/**
 * Bidelimited expressions are those that do not need to
 * be wrapped in parentheses in an opseq. In most cases,
 * this means those expressions that don't have subexpressions
 * at the outer left/right edges in the concrete syntax.
 * In the ostensibly bidelimited case of case...end expressions,
 * however, we still require explicit parenthesization in an
 * opseq. This is because, in our edit actions, we require that
 * let and case expressions be constructed only at the beginning
 * of a line or parenthesized expression -- hence, constructing
 * a case expression in the middle of an opseq requires first
 * constructing parentheses around the desired scrutinee within
 * the opseq. For consistency, we require that case expressions
 * always be parenthesized in an opseq.
 */
let bidelimited =
  fun
  /* bidelimited cases */
  | Tm(_, EmptyHole(_))
  | Tm(_, Var(_, _))
  | Tm(_, NumLit(_))
  | Tm(_, BoolLit(_))
  | Tm(_, Inj(_, _))
  | Tm(_, ListNil)
  /* | Tm _ (ListLit _) */
  | Tm(_, ApPalette(_, _, _))
  | Parenthesized(_) => true
  /* non-bidelimited cases */
  | Tm(_, Case(_, _, _))
  | Tm(_, LineItem(_, _))
  | Tm(_, Lam(_, _, _))
  | Tm(_, OpSeq(_, _)) => false;

/* if e is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = e =>
  if (bidelimited(e)) {
    e;
  } else {
    Parenthesized(e);
  };

/* put e in the specified hole */
let rec set_err_status = err =>
  fun
  | Tm(_, OpSeq(Skel.BinOp(_, op, skel1, skel2), seq)) =>
    Tm(err, OpSeq(Skel.BinOp(err, op, skel1, skel2), seq))
  | Tm(_, e') => Tm(err, e')
  | Parenthesized(e') => Parenthesized(set_err_status(err, e'));

/* put e in a new hole, if it is not already in a hole */
let rec make_inconsistent = (u_gen, e) =>
  switch (e) {
  | Tm(NotInHole, _)
  | Tm(InHole(WrongLength, _), _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let e = set_err_status(InHole(TypeInconsistent, u), e);
    (e, u_gen);
  | Tm(InHole(TypeInconsistent, _), _) => (e, u_gen)
  | Parenthesized(e1) =>
    switch (make_inconsistent(u_gen, e1)) {
    | (e1', u_gen') => (Parenthesized(e1'), u_gen')
    }
  };

/* put skel in a new hole, if it is not already in a hole */
let make_skel_inconsistent =
    (u_gen: MetaVarGen.t, skel: skel_t, seq: opseq)
    : (skel_t, opseq, MetaVarGen.t) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let (en', u_gen') = make_inconsistent(u_gen, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en')) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq') => (skel, seq', u_gen')
      };
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) => (skel, seq, u_gen)
  | Skel.BinOp(NotInHole, op, skel1, skel2)
  | Skel.BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (Skel.BinOp(InHole(TypeInconsistent, u), op, skel1, skel2), seq, u_gen);
  };

let rec drop_outer_parentheses = e =>
  switch (e) {
  | Tm(_, _) => e
  | Parenthesized(e') => drop_outer_parentheses(e')
  };

let prune_single_hole_line = li =>
  switch (li) {
  | ExpLine(Tm(_, EmptyHole(_))) => EmptyLine
  | ExpLine(_)
  | EmptyLine
  | LetLine(_, _, _) => li
  };

let prepend_line = (li: line_item, e: t): t =>
  Tm(NotInHole, LineItem(li, e));
