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
type block =
  | Block(err_status, lines, t)
and lines = list(line)
and line =
  | EmptyLine
  | LetLine(UHPat.t, option(UHTyp.t), block)
  | ExpLine(t)
and t =
  | Parenthesized(block)
  | EmptyHole(MetaVar.t)
  | OpSeq(skel_t, opseq) /* invariant: skeleton is consistent with opseq */
  | Tm(err_status, t')
and t' =
  | Var(var_err_status, Var.t)
  | Lam(UHPat.t, option(UHTyp.t), block)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(inj_side, block)
  | Case(block, rules, option(UHTyp.t))
  | ListNil
  | ApPalette(PaletteName.t, SerializedModel.t, splice_info)
and opseq = OperatorSeq.opseq(t, op)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, block)
and splice_info = SpliceInfo.t(block)
and splice_map = SpliceInfo.splice_map(block);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

let rec get_tuple = (skel1: skel_t, skel2: skel_t): ListMinTwo.t(skel_t) =>
  switch (skel2) {
  | BinOp(_, Comma, skel21, skel22) =>
    Cons(skel1, get_tuple(skel21, skel22))
  | BinOp(_, _, _, _)
  | Placeholder(_) => Pair(skel1, skel2)
  };

let rec make_tuple = (err: err_status, skels: ListMinTwo.t(skel_t)): skel_t =>
  switch (skels) {
  | Pair(skel1, skel2) => BinOp(err, Comma, skel1, skel2)
  | Cons(skel1, skels) =>
    BinOp(err, Comma, skel1, make_tuple(NotInHole, skels))
  };

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (EmptyHole(u), u_gen);
};

let is_EmptyHole = (e: t): bool =>
  switch (e) {
  | EmptyHole(_) => true
  | _ => false
  };

let empty_rule = (u_gen: MetaVarGen.t): (rule, MetaVarGen.t) => {
  let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
  let (e, u_gen) = new_EmptyHole(u_gen);
  let block = Block(NotInHole, [], e);
  let rule = Rule(p, block);
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
let bidelimited = (e: t): bool =>
  switch (e) {
  /* bidelimited cases */
  | EmptyHole(_)
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
  | Tm(_, Lam(_, _, _))
  | OpSeq(_, _) => false
  };

/* if e is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = (e: t): t =>
  if (bidelimited(e)) {
    e;
  } else {
    Parenthesized(Block(NotInHole, [], e));
  };

let set_err_status_block =
    (err: err_status, Block(_, lines, e): block): block =>
  Block(err, lines, e);
/* put e in the specified hole */
let rec set_err_status_t = (err: err_status, e: t): t =>
  switch (e) {
  | Tm(_, e') => Tm(err, e')
  | Parenthesized(block) => Parenthesized(set_err_status_block(err, block))
  | OpSeq(skel, seq) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    OpSeq(skel, seq);
  | EmptyHole(_) => e
  }
and set_err_status_opseq =
    (err: err_status, skel: skel_t, seq: opseq): (skel_t, opseq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let en = set_err_status_t(err, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq)
      };
    }
  | BinOp(_, op, skel1, skel2) => (BinOp(err, op, skel1, skel2), seq)
  };

let rec make_block_inconsistent =
        (u_gen: MetaVarGen.t, block: block): (block, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  let block = set_err_status_block(InHole(TypeInconsistent, u), block);
  (block, u_gen);
}
/* put e in a new hole, if it is not already in a hole */
and make_t_inconsistent = (u_gen: MetaVarGen.t, e: t): (t, MetaVarGen.t) =>
  switch (e) {
  | Tm(NotInHole, _)
  | Tm(InHole(WrongLength, _), _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let e = set_err_status_t(InHole(TypeInconsistent, u), e);
    (e, u_gen);
  | Tm(InHole(TypeInconsistent, _), _) => (e, u_gen)
  | Parenthesized(block) =>
    let (block, u_gen) = make_block_inconsistent(u_gen, block);
    (Parenthesized(block), u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, u_gen) = make_opseq_inconsistent(u_gen, skel, seq);
    (OpSeq(skel, seq), u_gen);
  | EmptyHole(_) => (e, u_gen)
  }
/* put skel in a new hole, if it is not already in a hole */
and make_opseq_inconsistent =
    (u_gen: MetaVarGen.t, skel: skel_t, seq: opseq)
    : (skel_t, opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(en) =>
      let (en, u_gen) = make_t_inconsistent(u_gen, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en)) {
      | None => raise(SkelInconsistentWithOpSeq(skel, seq))
      | Some(seq) => (skel, seq, u_gen)
      };
    }
  | BinOp(InHole(TypeInconsistent, _), _, _, _) => (skel, seq, u_gen)
  | BinOp(NotInHole, op, skel1, skel2)
  | BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    (BinOp(InHole(TypeInconsistent, u), op, skel1, skel2), seq, u_gen);
  };

/*
 let rec drop_outer_parentheses = (e: t): block =>
   switch (e) {
   | Tm(_, _)
   | OpSeq(_, _)
   | EmptyHole(_) => Block([], e)
   | Parenthesized(block) => drop_outer_parentheses(e')
   };
 */
