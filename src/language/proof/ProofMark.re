open Util;

/* The ways a proof step can go wrong during checking (ProofCheck), as
   opposed to the statics' `Mark.t`. Stored in `ProofMap.entry.marks` and
   rendered like statics errors. */

/* Declaration order is the priority order (for the cursor inspector and
   error selection): earlier constructors have higher priority, as in
   `Mark.re`. */
[@deriving (show({with_path: false}), sexp, yojson, variants)]
type t =
  /* Propagation break: this step can't act because the incoming goal is
   * missing (an earlier step failed, or we're inside an unprovable
   * forall). */
  | MissingIncoming
  /* The proof term itself is malformed syntax (Invalid/MultiHole) —
   * distinct from an EmptyHole, which is an intentionally-incomplete
   * proof and carries no mark. */
  | MalformedProofTerm
  /* Axiom: equality argument isn't a resolvable name. */
  | MalformedEqualityName
  /* Axiom: the named equality rule is not in scope. */
  | UnknownEquality(string)
  /* Axiom: the rule exists but doesn't unify with the target in the
   * chosen direction. */
  | RuleDoesNotApply({
      equality: string,
      direction: Direction.t,
    })
  /* Axiom: a rule matched, but the match and any `with` clause left some
   * of its binders unassigned, so the rewrite would bring them into the
   * goal free. */
  | UnderdeterminedInstantiation({equality: string})
  /* Axiom / revert: a `with <var> = <exp>` clause named something that
   * is not a binder of the cited rule (axiom) or of the reverted fact
   * (revert). The goal passes through, as with the other citation marks.
   * `var` is the offending name, or "?" for a non-variable slot. */
  | UnknownInstantiationVar({
      equality: string,
      var: string,
    })
  /* Revert: a `with` clause was supplied but the in-scope fact has no
   * `forall` to eliminate. */
  | RevertFactNotQuantified({var: string})
  /* Axiom / revert: a metavariable instantiation could not be shown
   * structurally total (Totality.check). Divergence is ⊥, which no
   * boolean obligation can rule out. `var` is the rule metavariable whose
   * instantiation failed. */
  | PossiblyDivergentInstantiation({
      equality: string,
      var: string,
    })
  /* Algebrite: Float-typed rewrites are refused, because the CAS's field
   * laws are false for IEEE floats. */
  | FloatAlgebrite
  /* Rewrite steps: `at_idx` isn't a concrete int literal. */
  | MalformedIndex
  /* Rewrite steps: `at_exp` pattern not found at the requested
   * occurrence index in the incoming goal. */
  | PatternNotFound({
      at_exp: Exp.t,
      at_idx: int,
    })
  /* Eval: the occurrence was located but `step_fn` had nothing
   * reducible to take. */
  | NothingToStep({at_exp: Exp.t})
  /* Forall: incoming goal isn't a `forall`/`fun`-binder. */
  | ExpectedForallGoal
  /* Funext: incoming goal isn't an equality (`==`) between two things of
     arrow type, so there is no pointwise goal to form. `==` on a list of
     functions is not extensionality. */
  | ExpectedFunEqGoal
  /* Funext: the binder is not a pattern that can be added to the semantic
     context (so the pointwise goal would have a free/unbindable name). */
  | MalformedFunext
  /* Generalize: the argument isn't a bare in-scope variable (a peeled
   * binder that can be re-quantified). */
  | MalformedGeneralize
  /* Revert: no in-scope fact matches the reverted expression. Less
   * urgent than a broken axiom citation, more urgent than the structural
   * induction marks. */
  | UnknownFactReverted
  /* Contradiction: no in-scope fact matches the cited expression. The
     goal passes through, as for `UnknownFactReverted`. */
  | UnknownFactContradicted
  /* Contradiction: the fact was found, but after the step's own
     `with <var> = <exp>` rewrite (if any) it did not evaluate to `false`:
     it computed to `true`, got stuck (typically because it is still
     open), or ran out of fuel. The goal passes through. */
  | ContradictionNotFalse
  /* Contradiction: a `with <var> = <exp>` clause was supplied, but the
     equation licensing it (`<var> == <exp>`, either orientation) is not
     an in-scope fact, or `<var>` is not a bare variable. The step
     substitutes only what the branch already knows, so the goal passes
     through. `var` is the offending name, or "?" for a non-variable
     slot. */
  | ContradictionSubstitutionUnverified({var: string})
  /* Induction: cases don't cover the scrutinee's type. */
  | InductionNotExhaustive
  /* Induction: the scrutinee's type could not be positively determined
   * (no statics info, or `Unknown` after normalization — an unannotated
   * quantifier binder, a type hole, an alias resolving to a hole), so
   * there is no constructor set for the cases to exhaust. The case
   * split's soundness argument needs to know what the scrutinee ranges
   * over: `| true | false` only reduces `forall b -> P(b)` to
   * `P(true) && P(false)` if `b` really is a boolean, so the checker
   * doesn't guess a type from the patterns. Distinct from
   * `InductionNotExhaustive` because the remedy is to annotate the
   * binder, not to add a case. */
  | InductionScrutineeUntyped
  /* Induction/split: a computed scrutinee (bool split) could not be
   * shown structurally total, as for `PossiblyDivergentInstantiation`.
   * Structural induction on a bare quantified variable never carries
   * this mark. */
  | PossiblyDivergentScrutinee
  | InductionEmptyCases
  /* Induction: a case pattern contains a wildcard, which has no case
   * equation, so no case of the split is checked. */
  | InductionUnsupportedWildcard;

let compare = (a: t, b: t): int =>
  Int.compare(Variants.to_rank(a), Variants.to_rank(b));

let highest = (marks: list(t)): option(t) =>
  switch (marks) {
  | [] => None
  | [h, ...tl] =>
    Some(
      List.fold_left(
        (best, cur) => compare(cur, best) < 0 ? cur : best,
        h,
        tl,
      ),
    )
  };
