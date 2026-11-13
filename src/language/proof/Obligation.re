open Util;

/* The sequent-shaped proof obligations incurred by proof steps.
 *
 * An obligation is a sequent: a local context (the bindings visible at the
 * obligation site — obligations raised inside induction cases mention
 * case-local binders) plus a boolean goal. Obligations are *computed* by
 * the checker on every pass and recorded in the ProofMap entry of the step
 * that incurred them; they are never stored in program text.
 *
 * `discharge` records what discharged the obligation, if anything. */

[@deriving (show({with_path: false}), sexp, yojson)]
type discharge =
  /* Discharged by syntactic lookup of an in-scope fact (binder
   * restriction, case_eq, hypothesis), identified by the fact's id. */
  | Remote(Id.t)
  /* The goal was closed and evaluated to `true`, so the stepper can
   * replay it. */
  | Evaluated
  | Pending;

/* Why a step incurs the obligation. */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  /* A term the step introduces is defined: a divisor is nonzero, a
   * list is nonempty (DomainConditions). */
  | Definedness
  /* A function definition's body is defined wherever its `where`
   * contract admits an argument. */
  | Contract
  /* A cited rule's assumption, `A` of `A ==> …`, as the match
   * instantiated it; or a premise of `have`'s cut rule. */
  | Premise
  /* An `assume` whose hypothesis isn't the goal's antecedent has to
   * establish it. */
  | Assumption;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* The proof step that incurred the obligation. */
  origin: Id.t,
  kind,
  /* The bindings visible at the obligation site. */
  bindings: list(Ctx.entry),
  /* As written: it names the program's definitions. */
  goal: Exp.t,
  discharge,
};

/* For a `Remote` discharge, the covering fact's name and statement,
 * found by id among the obligation's own bindings (`Ctx.TheoremEntry`s).
 * `None` for any other discharge, or if the id is not among the
 * bindings. */
let remote_fact = (ob: t): option((Var.t, Exp.t)) =>
  switch (ob.discharge) {
  | Remote(fact_id) =>
    ob.bindings
    |> List.find_map((e: Ctx.entry) =>
         switch (e) {
         | TheoremEntry({id, name, prop: Some(fact)})
             when Id.compare(id, fact_id) == 0 =>
           Some((name, fact))
         | _ => None
         }
       )
  | Evaluated
  | Pending => None
  };

let is_pending = (ob: t): bool =>
  switch (ob.discharge) {
  | Pending => true
  | Remote(_)
  | Evaluated => false
  };
