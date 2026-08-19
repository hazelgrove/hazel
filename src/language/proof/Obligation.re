open Util;

/* OBLIGATION.re — the sequent-shaped proof obligations incurred by proof
 * steps (design: docs/prover-obligations.md §3).
 *
 * An obligation is a sequent: a local context (the bindings visible at the
 * obligation site — obligations raised inside induction cases mention
 * case-local binders) plus a boolean goal. Obligations are *computed* by
 * the checker on every pass and recorded in the ProofMap entry of the step
 * that incurred them; they are never stored in program text.
 *
 * `discharge` records provenance: which in-scope fact covers the
 * obligation (channel 1, dumb syntactic lookup), a local inline subproof
 * (future), or nothing (`Pending`). */

[@deriving (show({with_path: false}), sexp, yojson)]
type discharge =
  /* Discharged by an inline subproof at the step — future work. */
  | Local(Id.t)
  /* Discharged by an in-scope fact (binder restriction, case_eq,
   * hypothesis), identified by that fact's stable id. */
  | Remote(Id.t)
  /* Channel 2 (docs/prover-obligations.md §4.2): the goal was closed and
   * evaluated to the literal `true`. Replayable in the stepper by
   * construction. */
  | Evaluated
  /* Nothing in scope covers it. */
  | Pending;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* The proof step that incurred the obligation. */
  origin: Id.t,
  /* Local context at the obligation site, sequent-style. */
  bindings: list(Ctx.entry),
  /* The boolean goal to be established. */
  goal: Exp.t,
  discharge,
};

let is_pending = (ob: t): bool =>
  switch (ob.discharge) {
  | Pending => true
  | Local(_)
  | Remote(_)
  | Evaluated => false
  };
