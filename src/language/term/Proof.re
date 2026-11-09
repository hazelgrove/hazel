[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Seq
  | AxiomStep
  | AlgebriteStep
  | EvalStep
  | Focus
  | Induction
  | Forall
  | Funext
  | Assume
  | Generalize
  | Revert
  | Contradiction
  | Have
  | Alias
  | Parens;

include TermBase.Proof;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): TermBase.Proof.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let rec cls_of_term: Grammar.proof_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Seq(_, _) => Seq
  /* As `Exp.cls_of_term` does, report the form inside the parens. */
  | Parens(p) => cls_of_term(p.term)
  | AxiomStep(_) => AxiomStep
  | AlgebriteStep(_) => AlgebriteStep
  | Induction(_, _, _) => Induction
  | Forall(_, _) => Forall
  | Funext(_, _) => Funext
  | Assume(_, _, _) => Assume
  | Generalize(_, _) => Generalize
  | Revert(_, _, _) => Revert
  | Contradiction(_) => Contradiction
  | Have(_, _, _) => Have
  | Alias(_, _, _) => Alias
  | EvalStep => EvalStep
  | Focus(_) => Focus;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid proof"
  | MultiHole => "Broken proof"
  | EmptyHole => "Empty proof hole"
  | Seq => "Proof sequence"
  | AxiomStep => "Axiom step"
  | AlgebriteStep => "Algebrite step"
  | Induction => "Induction step"
  | Forall => "Forall step"
  | Funext => "Funext step"
  | Assume => "Assume step"
  | Generalize => "Generalize step"
  | Revert => "Revert step"
  | Contradiction => "Contradiction step"
  | Alias => "Alias step"
  | Have => "Have step"
  | Parens => "Parenthesized proof"
  | EvalStep => "Eval step"
  | Focus => "Focus";

/* Ignores parens and projectors, as `Exp.fast_equal` does. */
let fast_equal =
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      ignore_projectors: true,
    })
  ).
    proof;

/* The sub-proofs directly inside a proof, in source order. */
let children = (p: t): list(t) =>
  switch (p.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | EvalStep
  | Contradiction(_) => []
  | Seq(p1, p2) => [p1, p2]
  | Focus({body, _})
  | Parens(body)
  | Forall(_, body)
  | Funext(_, body)
  | Assume(_, _, body)
  | Generalize(_, body)
  | Alias(_, _, body)
  | Revert(_, _, body) => [body]
  | Have(_, sub, body) => [sub, body]
  | Induction(_, _, cases) => List.map(snd, cases)
  };

/* Is any step of this proof a hole? Holes in a step's arguments don't
 * count; see `args_have_hole`. */
let rec has_hole = (p: t): bool =>
  switch (p.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => true
  | Seq(_)
  | Parens(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | EvalStep
  | Focus(_)
  | Induction(_)
  | Forall(_)
  | Funext(_)
  | Assume(_)
  | Generalize(_)
  | Revert(_)
  | Contradiction(_)
  | Have(_)
  | Alias(_) => List.exists(has_hole, children(p))
  };

/* Do the step's own arguments contain a hole? Unlike `has_hole`, this
 * skips nested sub-proofs: the stepper shows a hole in a case body as its
 * own "…" row, but a hole in an argument (an induction pattern or
 * scrutinee, a forall binder, a focus's selector, an `as` name) has no
 * other indicator. */
let rec args_have_hole = (p: t): bool => {
  module M = {
    exception Found_hole;
  };
  let is_exp_hole = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => true
    | _ => false
    };
  let is_pat_hole = (p: Pat.t) =>
    switch (Pat.term_of(p)) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_) => true
    | _ => false
    };
  let raise_on_hole = (is_hole, cont, tm) =>
    is_hole(tm) ? raise(M.Found_hole) : cont(tm);
  let exp = (e: Exp.t): bool =>
    switch (
      Exp.map_term(
        ~f_exp=raise_on_hole(is_exp_hole),
        ~f_pat=raise_on_hole(is_pat_hole),
        e,
      )
    ) {
    | _ => false
    | exception M.Found_hole => true
    };
  let pat = (p: Pat.t): bool =>
    switch (Pat.map_term(~f_pat=raise_on_hole(is_pat_hole), p)) {
    | _ => false
    | exception M.Found_hole => true
    };
  let inst = (inst: option((Exp.t, Exp.t))): bool =>
    switch (inst) {
    | None => false
    | Some((v, i)) => exp(v) || exp(i)
    };
  let as_name = (as_name: option(Pat.t)): bool =>
    switch (as_name) {
    | None => false
    | Some(x) => pat(x)
    };
  switch (p.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | Seq(_, _) => false
  | Parens(p) => args_have_hole(p)
  | AxiomStep({equality, instantiation, direction: _}) =>
    exp(equality) || inst(instantiation)
  | AlgebriteStep(e) => exp(e)
  | EvalStep => false
  | Focus({selector, _}) => exp(selector)
  | Induction(scrut, x, cases) =>
    exp(scrut)
    || as_name(x)
    || List.exists(((case_pat, _)) => pat(case_pat), cases)
  | Forall(x, _)
  | Funext(x, _) => pat(x)
  | Assume(e, x, _) => exp(e) || as_name(x)
  | Alias(x, e, _) => pat(x) || exp(e)
  | Generalize(e, _)
  /* A have's subproof is a nested proof and reports its own holes. */
  | Have(e, _, _) => exp(e)
  | Revert(e, i, _)
  | Contradiction(e, i) => exp(e) || inst(i)
  };
};
