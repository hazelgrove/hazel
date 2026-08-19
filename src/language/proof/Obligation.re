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
  /* The boolean goal to be established. This is the SEMANTIC goal: the
   * environment has been substituted in (see
   * `ProofCheck.incur_obligation`), so discharge — lookup against binder
   * facts, closed evaluation — operates on closed-as-possible terms. */
  goal: Exp.t,
  /* The same goal BEFORE `Substitution.in_exp`, kept solely so the UI has
   * something readable to print. Env-inlining a guard like `y != 0` where
   * `y` is bound to a closure produces a goal whose printed form is a
   * whole closure dump — unreadable in the obligations panel even though
   * it is the right term to reason with. Rather than have the view attempt
   * to un-substitute (impossible in general), the checker records what the
   * user actually wrote at the obligation site. Additive and
   * display-only: nothing in the checker reads this field, and `goal`
   * remains the semantic one. */
  display_goal: Exp.t,
  discharge,
};

/* What to print for an obligation: the pre-substitution goal. */
let display_goal_of = (ob: t): Exp.t => ob.display_goal;

/* The receipt for a `Remote` discharge (docs/prover-obligations.md §4.2,
 * "receipts everywhere"): the covering fact's binder name and statement,
 * recovered from the obligation's own local context. Hypotheses are ctx
 * var entries typed `ProofOf(fact)` (`SemanticCtx.add_hypothesis`), and
 * `discharge` stores the entry's id, so no extra plumbing is needed to
 * show WHY an obligation is discharged. `None` for a non-Remote
 * discharge, or if the id is not among the recorded bindings. */
let remote_fact = (ob: t): option((Var.t, Exp.t)) =>
  switch (ob.discharge) {
  | Remote(fact_id) =>
    ob.bindings
    |> List.find_map((e: Ctx.entry) =>
         switch (e) {
         | VarEntry({id, name, typ, _}) when Id.compare(id, fact_id) == 0 =>
           switch (Typ.term_of(typ)) {
           | ProofOf(fact) => Some((name, fact))
           | _ => None
           }
         | _ => None
         }
       )
  | Local(_)
  | Evaluated
  | Pending => None
  };

let is_pending = (ob: t): bool =>
  switch (ob.discharge) {
  | Pending => true
  | Local(_)
  | Remote(_)
  | Evaluated => false
  };
