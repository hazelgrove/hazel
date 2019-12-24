open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type operator =
  | Space
  | Plus
  | Minus
  | Times
  | LessThan
  | GreaterThan
  | Equals
  | Comma
  | Cons
  | And
  | Or;

let string_of_operator =
  fun
  | Space => " "
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  | Comma => ","
  | Cons => "::"
  | And => "&&"
  | Or => "||";

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
  | E2(block)
  | E1(opseq)
  | E0(operand)
and block = list(line)
and line =
  | EmptyLine
  | LetLine(UHPat.t, option(UHTyp.t), t)
  | ExpLine(opseq)
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Lam(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(ErrStatus.t, t, rules, option(UHTyp.t))
  | Parenthesized(t)
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t)
and splice_info = SpliceInfo.t(t)
and splice_map = SpliceInfo.splice_map(t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

type affix = Seq.affix(operand, operator);

let letline = (p: UHPat.t, ~ann: option(UHTyp.t)=?, def: t): line =>
  LetLine(p, ann, def);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let numlit = (~err: ErrStatus.t=NotInHole, n: int): operand =>
  NumLit(err, n);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool): operand =>
  BoolLit(err, b);

let lam =
    (
      ~err: ErrStatus.t=NotInHole,
      p: UHPat.t,
      ~ann: option(UHTyp.t)=?,
      body: t,
    )
    : operand =>
  Lam(err, p, ann, body);

let case =
    (
      ~err: ErrStatus.t=NotInHole,
      ~ann: option(UHTyp.t)=?,
      scrut: t,
      rules: rules,
    )
    : operand =>
  Case(err, scrut, rules, ann);

let listnil = (~err: ErrStatus.t=NotInHole, ()): operand => ListNil(err);

let wrap_in_block = (opseq: opseq): block => [ExpLine(opseq)];

let prune_empty_hole_line = (li: line): line =>
  switch (li) {
  | ExpLine(OpSeq(_, S(EmptyHole(_), E))) => EmptyLine
  | ExpLine(_)
  | EmptyLine
  | LetLine(_, _, _) => li
  };
let prune_empty_hole_lines = List.map(prune_empty_hole_line);

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

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (operand, MetaVarGen.t) => {
  let (u, u_gen) = u_gen |> MetaVarGen.next;
  (EmptyHole(u), u_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let empty_rule = (u_gen: MetaVarGen.t): (rule, MetaVarGen.t) => {
  let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
  let (e, u_gen) = new_EmptyHole(u_gen);
  let rule = Rule(P0(p), E0(e));
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
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | Inj(_, _, _)
  | ApPalette(_, _, _, _)
  | Parenthesized(_) => true
  | Case(_, _, _, _)
  | Lam(_, _, _, _) => false;

/* if e is not bidelimited, bidelimit e parenthesizes it */
let bidelimit = (operand): operand =>
  if (bidelimited(operand)) {
    operand;
  } else {
    Parenthesized(E0(operand));
  };

let get_opseq =
  fun
  | EmptyLine
  | LetLine(_, _, _) => None
  | ExpLine(opseq) => Some(opseq);
let force_get_opseq = line =>
  line
  |> get_opseq
  |> Opt.get(_ => failwith("force_get_opseq: expected ExpLine"));

let split_conclusion = (block: block): option((list(line), opseq)) =>
  switch (block |> split_last) {
  | None => None
  | Some((leading, last)) =>
    switch (last |> get_opseq) {
    | None => None
    | Some(opseq) => Some((leading, opseq))
    }
  };
let force_split_conclusion = (block: block): (list(line), opseq) =>
  switch (block |> split_conclusion) {
  | None => failwith("force_split_conclusion: unconcluded block")
  | Some((leading, conclusion)) => (leading, conclusion)
  };

let join_conclusion = (leading: list(line), conclusion: opseq): block =>
  leading @ [ExpLine(conclusion)];

let rec get_err_status: t => ErrStatus.t =
  fun
  | E2(e2) => e2 |> get_err_status_block
  | E1(e1) => e1 |> get_err_status_opseq
  | E0(e0) => e0 |> get_err_status_operand
and get_err_status_block = block => {
  let (_, conclusion) = block |> force_split_conclusion;
  conclusion |> get_err_status_opseq;
}
and get_err_status_opseq = opseq =>
  OpSeq.get_err_status(~get_err_status_operand, opseq)
and get_err_status_operand =
  fun
  | EmptyHole(_) => NotInHole
  | Var(err, _, _)
  | NumLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | Lam(err, _, _, _)
  | Inj(err, _, _)
  | Case(err, _, _, _)
  | ApPalette(err, _, _, _) => err
  | Parenthesized(e) => get_err_status(e);

/* put e in the specified hole */
let rec set_err_status = (err: ErrStatus.t, e: t): t =>
  switch (e) {
  | E2(e2) => E2(e2 |> set_err_status_block(err))
  | E1(e1) => E1(e1 |> set_err_status_opseq(err))
  | E0(e0) => E0(e0 |> set_err_status_operand(err))
  }
and set_err_status_block = (err: ErrStatus.t, block: block): block => {
  let (leading, conclusion) = block |> force_split_conclusion;
  join_conclusion(leading, conclusion |> set_err_status_opseq(err));
}
and set_err_status_opseq = (err, opseq) =>
  OpSeq.set_err_status(~set_err_status_operand, err, opseq)
and set_err_status_operand = (err, operand) =>
  switch (operand) {
  | EmptyHole(_) => operand
  | Var(_, var_err, x) => Var(err, var_err, x)
  | NumLit(_, n) => NumLit(err, n)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Lam(_, p, ann, def) => Lam(err, p, ann, def)
  | Inj(_, inj_side, body) => Inj(err, inj_side, body)
  | Case(_, scrut, rules, ann) => Case(err, scrut, rules, ann)
  | ApPalette(_, name, model, si) => ApPalette(err, name, model, si)
  | Parenthesized(body) => Parenthesized(body |> set_err_status(err))
  };

let is_inconsistent = operand =>
  switch (operand |> get_err_status_operand) {
  | InHole(TypeInconsistent, _) => true
  | _ => false
  };

/* put e in a new hole, if it is not already in a hole */
let rec make_inconsistent = (u_gen: MetaVarGen.t, e: t): (t, MetaVarGen.t) =>
  switch (e) {
  | E2(e2) =>
    let (e2, u_gen) = e2 |> make_inconsistent_block(u_gen);
    (E2(e2), u_gen);
  | E1(e1) =>
    let (e1, u_gen) = e1 |> make_inconsistent_opseq(u_gen);
    (E1(e1), u_gen);
  | E0(e0) =>
    let (e0, u_gen) = e0 |> make_inconsistent_operand(u_gen);
    (E0(e0), u_gen);
  }
and make_inconsistent_block =
    (u_gen: MetaVarGen.t, block: block): (block, MetaVarGen.t) => {
  let (leading, conclusion) = block |> force_split_conclusion;
  let (conclusion, u_gen) = conclusion |> make_inconsistent_opseq(u_gen);
  (join_conclusion(leading, conclusion), u_gen);
}
and make_inconsistent_opseq = (u_gen, opseq) =>
  OpSeq.make_inconsistent(~make_inconsistent_operand, u_gen, opseq)
and make_inconsistent_operand = (u_gen, operand) =>
  switch (operand) {
  /* already in hole */
  | EmptyHole(_)
  | Var(InHole(TypeInconsistent, _), _, _)
  | NumLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(InHole(TypeInconsistent, _), _, _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _) => (operand, u_gen)
  /* not in hole */
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | NumLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Lam(NotInHole | InHole(WrongLength, _), _, _, _)
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | Case(NotInHole | InHole(WrongLength, _), _, _, _)
  | ApPalette(NotInHole | InHole(WrongLength, _), _, _, _) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let operand =
      operand |> set_err_status_operand(InHole(TypeInconsistent, u));
    (operand, u_gen);
  /* err in constructor args */
  | Parenthesized(body) =>
    let (body, u_gen) = body |> make_inconsistent(u_gen);
    (Parenthesized(body), u_gen);
  };

let rec drop_outer_parentheses = (operand): t =>
  switch (operand) {
  | Parenthesized(E0(operand)) => drop_outer_parentheses(operand)
  | Parenthesized(e) => e
  | _ => E0(operand)
  };

let child_indices_line =
  fun
  | EmptyLine => []
  | ExpLine(_) => []
  | LetLine(_, None, _) => [0, 2]
  | LetLine(_, Some(_), _) => [0, 1, 2];
let child_indices_operand =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_) => []
  | Lam(_, _, None, _) => [0, 2]
  | Lam(_, _, Some(_), _) => [0, 1, 2]
  | Case(_, _, rules, None) => range(List.length(rules) + 1)
  | Case(_, _, rules, Some(_)) => range(List.length(rules) + 2)
  | Inj(_, _, _) => [0]
  | Parenthesized(_) => [0]
  | ApPalette(_, _, _, _) => [];
let child_indices_rule =
  fun
  | Rule(_, _) => [0, 1];

let num_lines: block => int = List.length;

let favored_child_of_line: line => option((ChildIndex.t, t)) =
  fun
  | EmptyLine
  | ExpLine(_) => None
  | LetLine(_, _, def) => Some((2, def));

let favored_child_of_operand: operand => option((ChildIndex.t, t)) =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | ApPalette(_, _, _, _) => None
  | Lam(_, _, _, e) => Some((2, e))
  | Inj(_, _, e)
  | Case(_, e, _, _)
  | Parenthesized(e) => Some((0, e));

let has_concluding_let_line = (block: block): bool => {
  let (leading, conclusion) = block |> force_split_conclusion;
  switch (leading |> split_last, conclusion) {
  | (Some((_, LetLine(_, _, _))), OpSeq(_, S(EmptyHole(_), E))) => true
  | (_, _) => false
  };
};

let rec is_multi_line: t => bool =
  fun
  | E2(e2) => e2 |> is_multi_line_block
  | E1(e1) => e1 |> is_multi_line_opseq
  | E0(e0) => e0 |> is_multi_line_operand
and is_multi_line_block = block =>
  List.length(block) > 1 || block |> List.exists(is_multi_line_line)
and is_multi_line_line =
  fun
  | EmptyLine => false
  | ExpLine(opseq) => is_multi_line_opseq(opseq)
  | LetLine(_, _, def) => is_multi_line(def)
and is_multi_line_opseq = (opseq: opseq): bool =>
  OpSeq.is_multi_line(~is_multi_line_operand, opseq)
and is_multi_line_operand =
  fun
  | EmptyHole(_)
  | Var(_, _, _)
  | NumLit(_, _)
  | BoolLit(_, _)
  | ListNil(_)
  | ApPalette(_, _, _, _) => false
  | Lam(_, _, _, body) => is_multi_line(body)
  | Inj(_, _, body) => is_multi_line(body)
  | Case(_, _, _, _) => true
  | Parenthesized(body) => is_multi_line(body);

let is_trivial_block =
  fun
  | [ExpLine(OpSeq(_, S(EmptyHole(_), E)))] => true
  | _ => false;
