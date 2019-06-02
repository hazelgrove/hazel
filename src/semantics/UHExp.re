open Sexplib.Std;
open SemanticsCommon;
open GeneralUtil;

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
  | Block(lines, t)
and lines = list(line)
and line =
  | ExpLine(t)
  | EmptyLine
  | LetLine(UHPat.t, option(UHTyp.t), block)
and t =
  /* outer nodes */
  | EmptyHole(MetaVar.t)
  | Var(err_status, var_err_status, Var.t)
  | NumLit(err_status, int)
  | BoolLit(err_status, bool)
  | ListNil(err_status)
  /* inner nodes */
  | Lam(err_status, UHPat.t, option(UHTyp.t), block)
  | Inj(err_status, inj_side, block)
  | Case(err_status, block, rules, option(UHTyp.t))
  | Parenthesized(block)
  | OpSeq(skel_t, opseq) /* invariant: skeleton is consistent with opseq */
  | ApPalette(err_status, PaletteName.t, SerializedModel.t, splice_info)
and opseq = OperatorSeq.opseq(t, op)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, block)
and splice_info = SpliceInfo.t(block)
and splice_map = SpliceInfo.splice_map(block);

exception SkelInconsistentWithOpSeq(skel_t, opseq);

/*
 let line_outer_length = (EmptyLine: line_outer): int => 0;
 let t_outer_length = (eo: t_outer): int =>
   switch (eo) {
   | EmptyHole(_) => 1
   | Var(_, _, x) => Var.length(x)
   | NumLit(_, n) => num_digits(n)
   | BoolLit(_, true) => 4
   | BoolLit(_, false) => 5
   | ListNil(_) => 2
   };
 */

let wrap_in_block = (e: t): block => Block([], e);

let prune_empty_hole_line = (li: line): line =>
  switch (li) {
  | ExpLine(EmptyHole(_)) => EmptyLine
  | ExpLine(_)
  | EmptyLine
  | LetLine(_, _, _) => li
  };
let prune_empty_hole_lines: lines => lines = List.map(prune_empty_hole_line);

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
  let block = wrap_in_block(e);
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
  /* bidelimited */
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | Inj(_, _, _)
  | ApPalette(_, _, _, _)
  | Parenthesized(_) => true
  /* non-bidelimited */
  | Case(_, _, _, _)
  | Lam(_, _, _, _)
  | OpSeq(_, _) => false
  };

/* if e is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = (e: t): t =>
  if (bidelimited(e)) {
    e;
  } else {
    Parenthesized(wrap_in_block(e));
  };

let rec get_err_status_block = (Block(_, e): block): err_status =>
  get_err_status_t(e)
and get_err_status_t = (e: t): err_status =>
  switch (e) {
  | EmptyHole(_) => NotInHole
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | Lam(err, _, _, _)
  | Inj(err, _, _)
  | Case(err, _, _, _)
  | ApPalette(err, _, _, _) => err
  | Parenthesized(block) => get_err_status_block(block)
  | OpSeq(BinOp(err, _, _, _), _) => err
  | OpSeq(Placeholder(n) as skel, seq) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
    | None => raise(SkelInconsistentWithOpSeq(skel, seq))
    | Some(e_n) => get_err_status_t(e_n)
    }
  };

let rec set_err_status_block =
        (err: err_status, Block(lines, e): block): block =>
  Block(lines, set_err_status_t(err, e))
/* put e in the specified hole */
and set_err_status_t = (err: err_status, e: t): t =>
  switch (e) {
  | EmptyHole(_) => e
  | Var(_, var_err, x) => Var(err, var_err, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Lam(_, p, ann, block) => Lam(err, p, ann, block)
  | Inj(_, inj_side, block) => Inj(err, inj_side, block)
  | Case(_, block, rules, ann) => Case(err, block, rules, ann)
  | ApPalette(_, name, model, si) => ApPalette(err, name, model, si)
  | Parenthesized(block) => Parenthesized(set_err_status_block(err, block))
  | OpSeq(skel, seq) =>
    let (skel, seq) = set_err_status_opseq(err, skel, seq);
    OpSeq(skel, seq);
  }
and set_err_status_opseq =
    (err: err_status, skel: skel_t, seq: opseq): (skel_t, opseq) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
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
  /* already in hole */
  | EmptyHole(_)
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(InHole(TypeInconsistent, _), _, _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _) => (e, u_gen)
  /* not in hole */
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | NumLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Lam(NotInHole | InHole(WrongLength, _), _, _, _)
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | Case(NotInHole | InHole(WrongLength, _), _, _, _)
  | ApPalette(NotInHole | InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = MetaVarGen.next(u_gen);
    let e = set_err_status_t(InHole(TypeInconsistent, u), e);
    (e, u_gen);
  /* err_status in constructor args */
  | Parenthesized(block) =>
    let (block, u_gen) = make_block_inconsistent(u_gen, block);
    (Parenthesized(block), u_gen);
  | OpSeq(skel, seq) =>
    let (skel, seq, u_gen) = make_opseq_inconsistent(u_gen, skel, seq);
    (OpSeq(skel, seq), u_gen);
  }
/* put skel in a new hole, if it is not already in a hole */
and make_opseq_inconsistent =
    (u_gen: MetaVarGen.t, skel: skel_t, seq: opseq)
    : (skel_t, opseq, MetaVarGen.t) =>
  switch (skel) {
  | Placeholder(n) =>
    switch (OperatorSeq.nth_tm(n, seq)) {
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

let rec drop_outer_parentheses = (e: t): block =>
  switch (e) {
  | Parenthesized(Block([], e)) => drop_outer_parentheses(e)
  | Parenthesized(block) => block
  | _ => Block([], e)
  };
