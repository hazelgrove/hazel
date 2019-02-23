open SemanticsCommon;
open Util;

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

type skel_t = Skel.t(op);

type t =
  | Tm(err_status, t')
  | Parenthesized(t)
and t' =
  | Asc(t, UHTyp.t)
  | Var(var_err_status, Var.t)
  | LineItem(line_item, t)
  | Lam(UHPat.t, option(UHTyp.t), t)
  | NumLit(int)
  | BoolLit(bool)
  | Inj(inj_side, t)
  | Case(t, list(rule))
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

let rec get_tuple = (skel1, skel2) =>
  switch (skel2) {
  | Skel.BinOp(_, Comma, skel21, skel22) => [
      skel1,
      ...get_tuple(skel21, skel22),
    ]
  | Skel.BinOp(_, _, _, _)
  | Skel.Placeholder(_) => [skel1, skel2]
  };

let rec make_tuple = err =>
  fun
  | [skel1, skel2] => Some(Skel.BinOp(err, Comma, skel1, skel2))
  | [skel1, ...skels] =>
    switch (make_tuple(NotInHole, skels)) {
    | None => None
    | Some(skel2) => Some(Skel.BinOp(err, Comma, skel1, skel2))
    }
  | [] => None;

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

/* bidelimited expressions are those that don't have
 * sub-expressions at their outer left or right edge
 * in the concrete syntax */
let bidelimited =
  fun
  /* bidelimited cases */
  | Tm(_, EmptyHole(_))
  | Tm(_, Var(_, _))
  | Tm(_, NumLit(_))
  | Tm(_, BoolLit(_))
  | Tm(_, Inj(_, _))
  | Tm(_, Case(_, _))
  | Tm(_, ListNil)
  /* | Tm _ (ListLit _) */
  | Tm(_, ApPalette(_, _, _))
  | Parenthesized(_) => true
  /* non-bidelimited cases */
  | Tm(_, Asc(_, _))
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
let make_skel_inconsistent = (u_gen, skel, seq) =>
  switch (skel) {
  | Skel.Placeholder(n) =>
    switch (OperatorSeq.seq_nth(n, seq)) {
    | Some(en) =>
      let (en', u_gen') = make_inconsistent(u_gen, en);
      switch (OperatorSeq.seq_update_nth(n, seq, en')) {
      | Some(seq') => Some((skel, seq', u_gen'))
      | None => None
      };
    | None => None
    }
  | Skel.BinOp(InHole(TypeInconsistent, _), _, _, _) =>
    Some((skel, seq, u_gen))
  | Skel.BinOp(NotInHole, op, skel1, skel2)
  | Skel.BinOp(InHole(WrongLength, _), op, skel1, skel2) =>
    let (u', u_gen') = MetaVarGen.next(u_gen);
    Some((
      Skel.BinOp(InHole(TypeInconsistent, u'), op, skel1, skel2),
      seq,
      u_gen',
    ));
  };

let rec drop_outer_parentheses = e =>
  switch (e) {
  | Tm(_, _) => e
  | Parenthesized(e') => drop_outer_parentheses(e')
  };

let rec prune_single_hole_lines = e =>
  switch (e) {
  | Tm(err_status, LineItem(li, e1)) =>
    let li = prune_single_hole_line(li);
    let e1 = prune_single_hole_lines(e1);
    Tm(err_status, LineItem(li, e1));
  | Tm(_, _)
  | Parenthesized(_) => e
  }
and prune_single_hole_line = li =>
  switch (li) {
  | ExpLine(Tm(_, EmptyHole(_))) => EmptyLine
  | ExpLine(_)
  | EmptyLine
  | LetLine(_, _, _) => li
  };