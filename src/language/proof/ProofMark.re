open Util;

/* PROOFMARK.re — error marks produced at evaluation time by ProofCheck.
   Distinct from statics `Mark.t` (which is produced by Statics.mk). These
   marks describe ways a proof step went wrong during checking — e.g. an
   axiom's named equality couldn't be resolved, a step's `at_exp` pattern
   wasn't found in the incoming goal, or an `eval` step had nothing to
   reduce. They ride along the existing `ProofMap` plumbing (stored inside
   `ProofMap.entry.marks`) and are rendered as red shards / red cursor
   inspector styling analogously to statics errors. */

/* NOTE: Declaration order is load-bearing.
   The priority of a mark (for cursor inspector / error selection) is
   determined by its position in this type: earlier-declared constructors
   have higher priority, mirroring the convention in `Mark.re`. */
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
  /* Step helpers: `at_idx` isn't a concrete int literal. */
  | MalformedIndex
  /* Step helpers: `at_exp` pattern not found at the requested
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
  /* Induction: cases don't cover the scrutinee's type. */
  | InductionNotExhaustive
  /* Induction: zero cases were given. */
  | InductionEmptyCases;

/* Declaration-order tag index, derived by ppx_variants_conv. */
let compare = (a: t, b: t): int =>
  Int.compare(Variants.to_rank(a), Variants.to_rank(b));

/* Earliest-declared variant wins (highest priority). */
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
