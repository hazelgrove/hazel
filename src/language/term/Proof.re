[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Seq
  | AxiomStep
  | AlgebriteStep
  | EvalStep
  | Induction
  | Forall
  | Assume;

include TermBase.Proof;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): TermBase.Proof.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.proof_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Seq(_, _) => Seq
  | AxiomStep(_) => AxiomStep
  | AlgebriteStep(_) => AlgebriteStep
  | Induction(_, _) => Induction
  | Forall(_, _) => Forall
  | Assume(_, _) => Assume
  | EvalStep(_) => EvalStep;

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
  | Assume => "Assume step"
  | EvalStep => "Eval step";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };

let rec fast_equal = (p1: t, p2: t): bool => {
  switch (p1.term, p2.term) {
  | (EmptyHole, EmptyHole) => true
  | (Invalid(s1), Invalid(s2)) => s1 == s2
  | (MultiHole(xs), MultiHole(ys)) =>
    List.length(xs) == List.length(ys)
    && List.for_all2(Equality.syntactic.any, xs, ys)
  | (Seq(a1, a2), Seq(b1, b2)) => fast_equal(a1, b1) && fast_equal(a2, b2)
  | (
      AxiomStep({at_idx: i1, at_exp: e1, direction: d1, equality: q1}),
      AxiomStep({at_idx: i2, at_exp: e2, direction: d2, equality: q2}),
    ) =>
    Equality.syntactic.exp(i1, i2)
    && Equality.syntactic.exp(e1, e2)
    && d1 == d2
    && Equality.syntactic.exp(q1, q2)
  | (
      AlgebriteStep({at_idx: i1, at_exp: e1, with_exp: w1}),
      AlgebriteStep({at_idx: i2, at_exp: e2, with_exp: w2}),
    ) =>
    Equality.syntactic.exp(i1, i2)
    && Equality.syntactic.exp(e1, e2)
    && Equality.syntactic.exp(w1, w2)
  | (EvalStep({at_idx: i1, at_exp: e1}), EvalStep({at_idx: i2, at_exp: e2})) =>
    Equality.syntactic.exp(i1, i2) && Equality.syntactic.exp(e1, e2)
  | (Induction(e1, cs1), Induction(e2, cs2)) =>
    Equality.syntactic.exp(e1, e2)
    && List.length(cs1) == List.length(cs2)
    && List.for_all2(
         ((pa, ba), (pb, bb)) =>
           Equality.syntactic.pat(pa, pb) && fast_equal(ba, bb),
         cs1,
         cs2,
       )
  | (Forall(x1, b1), Forall(x2, b2)) =>
    Equality.syntactic.pat(x1, x2) && fast_equal(b1, b2)
  | (Assume(e1, b1), Assume(e2, b2)) =>
    Equality.syntactic.exp(e1, e2) && fast_equal(b1, b2)
  | (EmptyHole, _)
  | (Invalid(_), _)
  | (MultiHole(_), _)
  | (Seq(_, _), _)
  | (AxiomStep(_), _)
  | (AlgebriteStep(_), _)
  | (Induction(_, _), _)
  | (Forall(_, _), _)
  | (Assume(_, _), _)
  | (EvalStep(_), _) => false
  };
};

let equal = fast_equal;

/* Does this proof (anywhere inside) contain an EmptyHole / Invalid /
 * MultiHole? */
let rec has_hole = (p: t): bool =>
  switch (p.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => true
  | Seq(p1, p2) => has_hole(p1) || has_hole(p2)
  | AxiomStep(_)
  | AlgebriteStep(_) => false
  | Induction(_, cases) =>
    List.exists(((_, body)) => has_hole(body), cases)
  | Forall(_, body) => has_hole(body)
  | Assume(_, body) => has_hole(body)
  | EvalStep(_) => false
  };

exception Found_hole;

let exp_has_hole = (e: Exp.t): bool => {
  let raise_on_hole = (is_hole, cont, tm) =>
    is_hole(tm) ? raise(Found_hole) : cont(tm);
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
  switch (
    Exp.map_term(
      ~f_exp=raise_on_hole(is_exp_hole),
      ~f_pat=raise_on_hole(is_pat_hole),
      e,
    )
  ) {
  | _ => false
  | exception Found_hole => true
  };
};

let pat_has_hole = (p: Pat.t): bool =>
  switch (
    Pat.map_term(
      ~f_pat=
        (cont, p) =>
          switch (Pat.term_of(p)) {
          | EmptyHole
          | Invalid(_)
          | MultiHole(_) => raise(Found_hole)
          | _ => cont(p)
          },
      p,
    )
  ) {
  | _ => false
  | exception Found_hole => true
  };

/* Do the step's OWN arguments contain a hole? Unlike `has_hole`, nested
 * sub-proofs are NOT inspected: a hole in a case body renders its own
 * "…" continuation row in the stepper, while a hole in an argument
 * position (an induction case pattern or scrutinee, a forall binder, an
 * axiom's target) has no other visible indicator. */
let args_have_hole = (p: t): bool =>
  switch (p.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | Seq(_, _) => false
  | AxiomStep({at_idx, at_exp, equality, direction: _}) =>
    exp_has_hole(at_idx) || exp_has_hole(at_exp) || exp_has_hole(equality)
  | AlgebriteStep({at_idx, at_exp, with_exp}) =>
    exp_has_hole(at_idx) || exp_has_hole(at_exp) || exp_has_hole(with_exp)
  | EvalStep({at_idx, at_exp}) =>
    exp_has_hole(at_idx) || exp_has_hole(at_exp)
  | Induction(scrut, cases) =>
    exp_has_hole(scrut)
    || List.exists(((pat, _)) => pat_has_hole(pat), cases)
  | Forall(pat, _) => pat_has_hole(pat)
  | Assume(e, _) => exp_has_hole(e)
  };
