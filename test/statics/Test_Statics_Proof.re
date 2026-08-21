open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Helpers to check that the statics result for a given program
   contains a mark matching a supplied predicate, or that it contains
   no errors of interest. */
let has_mark = (pred: Mark.t => bool, map: Statics.Map.t): bool =>
  Id.Map.exists(
    (_, info: Info.t) => List.exists(pred, Info.marks_of(info)),
    map,
  );

let no_mark = (pred: Mark.t => bool, map: Statics.Map.t): bool =>
  !has_mark(pred, map);

let expects_mark = (name: string, source: string, pred: Mark.t => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(source);
      let s = statics(exp);
      Alcotest.check(bool, name, true, has_mark(pred, s));
    },
  );

let expects_no_mark = (name: string, source: string, pred: Mark.t => bool) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(source);
      let s = statics(exp);
      Alcotest.check(bool, name, true, no_mark(pred, s));
    },
  );

let is_free_hyp =
  fun
  | Mark.FreeHypothesis(_) => true
  | _ => false;

let is_axiom_slot_error =
  fun
  | Mark.AxiomSlotNotHypothesis(_) => true
  | _ => false;

let is_free_var =
  fun
  | Mark.Free(_) => true
  | _ => false;

let is_expectation_mismatch =
  fun
  | Mark.ExpectationMismatch(_) => true
  | _ => false;

let is_redundant =
  fun
  | Mark.Redundant => true
  | _ => false;

let is_inexhaustive =
  fun
  | Mark.InexhaustiveMatch(_) => true
  | _ => false;

let tests = (
  "Statics.Proof",
  [
    /* Built-in axiom `refl_eq` is a hypothesis, so an axiom step
       referring to it should not produce a FreeHypothesis mark. */
    expects_no_mark(
      "axiom refl_eq is a known hypothesis",
      {|theorem t = true proof axiom refl_eq at 0 on 1 end in t|},
      is_free_hyp,
    ),
    /* A completely unknown name in the axiom equality slot should
       trigger FreeHypothesis. */
    expects_mark(
      "axiom bogus is a free hypothesis",
      {|theorem t = true proof axiom bogus at 0 on 1 end in t|},
      is_free_hyp,
    ),
    /* `forall x => ...`: `x` should be visible as a variable,
       not as a hypothesis. */
    expects_no_mark(
      "forall binds a variable visible in body",
      {|theorem t = true proof forall x => axiom refl_eq at 0 on x end in 0|},
      is_free_var,
    ),
    /* The theorem introduces the goal's outer `forall`-bound variables
       into the proof scope, so referencing `x` in the proof of
       `forall x -> x == x` is not a free variable. */
    expects_no_mark(
      "theorem goal forall var is visible in proof",
      {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in 0|},
      is_free_var,
    ),
    /* Proof induction must be exhaustive: a list induction with only the
       empty-list case is inexhaustive, even when the scrutinee is an
       unannotated forall variable (type Unknown refined from the patterns). */
    expects_mark(
      "induction on unannotated forall var flags inexhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs | [] => ? end in t|},
      is_inexhaustive,
    ),
    /* Covering both list constructors is exhaustive (no mark). */
    expects_no_mark(
      "induction covering [] and :: is exhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs | [] => ? | y :: ys => ? end in t|},
      is_inexhaustive,
    ),
    /* A proof induction with no cases proves nothing -> inexhaustive. */
    expects_mark(
      "induction with no cases flags inexhaustive",
      {|theorem t = forall xs -> xs == xs proof induction xs end in t|},
      is_inexhaustive,
    ),
    /* Induction with two identical patterns should flag redundancy. */
    expects_mark(
      "induction flags redundant rows",
      {|theorem t = true proof induction 1 | 1 => ? | 1 => ? end in t|},
      is_redundant,
    ),
    /* Induction with a non-exhaustive match should flag inexhaustiveness. */
    expects_mark(
      "induction flags inexhaustive match",
      {|theorem t = true proof induction 1 | 0 => ? end in t|},
      is_inexhaustive,
    ),
    /* `assume` introduces a hypothesis citable in its body: an axiom step
       naming the generated hypothesis (`assume`) is not a free
       hypothesis. */
    expects_no_mark(
      "assume body sees the hypothesis",
      {|theorem t = 1 == 1 proof assume 2 == 2 => axiom assume at 0 on 2 end in t|},
      is_free_hyp,
    ),
    /* The assumption analyzes against Bool: a non-bool assumption is a
       static type error. */
    expects_mark(
      "non-bool assumption flags a type error",
      {|theorem t = 1 == 1 proof assume 5 => ? in t|},
      is_expectation_mismatch,
    ),
    /* ...and a boolean assumption is not. */
    expects_no_mark(
      "bool assumption has no type error",
      {|theorem t = 1 == 1 proof assume 2 == 2 => ? in t|},
      is_expectation_mismatch,
    ),
    /* --- Phase 2: implication and restrictions ----------------------- */
    /* `==>` types Bool × Bool → Bool: boolean operands are fine... */
    expects_no_mark(
      "==> accepts boolean operands",
      {|theorem t = true ==> true proof ? in t|},
      is_expectation_mismatch,
    ),
    /* ...non-boolean ones are a type error. */
    expects_mark(
      "==> rejects non-boolean operands",
      {|theorem t = 5 ==> true proof ? in t|},
      is_expectation_mismatch,
    ),
    /* Restricted binder: the binder is in scope for both the guard and
       the body, and both analyze against Bool. */
    expects_no_mark(
      "forall-where binder scopes over guard and body",
      {|theorem t = forall x where x == 1 -> x == 1 proof ? in 0|},
      is_free_var,
    ),
    expects_mark(
      "forall-where guard analyzes against Bool",
      {|theorem t = forall x where 5 -> x == x proof ? in t|},
      is_expectation_mismatch,
    ),
    /* Function contract (Phase 3b): the parameter is in scope for both
       the guard and the body... */
    expects_no_mark(
      "fun-where binder scopes over guard and body",
      {|let f = fun x where x != 0 -> 100 / x in f(1) == f(1)|},
      is_free_var,
    ),
    /* ...and the guard analyzes against Bool. */
    expects_mark(
      "fun-where guard analyzes against Bool",
      {|let f = fun x where 5 -> x in f(1)|},
      is_expectation_mismatch,
    ),
    expects_no_mark(
      "fun-where bool guard has no type error",
      {|let f = fun x where x != 0 -> 100 / x in f(1)|},
      is_expectation_mismatch,
    ),
    /* Peeling a restricted binder installs the guard as a hypothesis
       (base name "where"): citing it in the proof is not a free
       hypothesis. */
    expects_no_mark(
      "forall-where restriction is citable as `where`",
      {|theorem t = forall x where x == 1 -> x == 1 proof axiom where at 0 on x end in 0|},
      is_free_hyp,
    ),
    /* --- proof/value namespace separation (2026-08-21) --------------
       docs/prover-obligations.md section 0.1. Theorems, axioms and
       hypotheses live in their OWN context namespace, like type
       aliases. Three consequences, pinned here. */
    /* (a) BEHAVIOR CHANGE. A theorem name is not a value, so naming it
       in expression position is a free variable. Before the separation
       `theorem t = ... in t` evaluated to a `ProofObject`. */
    expects_mark(
      "a theorem name in expression position is free",
      {|theorem t = true proof ? in t|},
      is_free_var,
    ),
    /* (b) ...and therefore a `let` and a `theorem` of the same name no
       longer collide: `t` in the body is the VALUE 5, not the theorem,
       and nothing is shadowed away. */
    expects_no_mark(
      "let-bound name and theorem name do not collide",
      {|let t = 5 in theorem t = true proof ? in t|},
      is_free_var,
    ),
    /* (c) ...and citation still resolves through the value namespace: a
       later `let` of the same name does not hide the theorem from an
       `axiom` slot, because the slot looks the name up in the theorem
       context. */
    expects_no_mark(
      "citation finds a theorem shadowed by a later value binding",
      {|theorem lem = 1 == 1 proof ? in let lem = 5 in theorem g = true proof axiom lem at 0 on 1 end in lem|},
      is_free_hyp,
    ),
    /* (d) Hypotheses are invisible to expressions for the same reason:
       the auto-named `assume` hypothesis is citable in a proof but is
       not a variable of the program. */
    expects_mark(
      "an assume hypothesis is not an expression variable",
      {|theorem t = 1 == 1 proof assume 2 == 2 => ? in assume|},
      is_free_var,
    ),
    /* --- Phase-5 hypothesis naming, at EDIT TIME ---------------------
       (docs/prover-obligations.md, "Hypothesis naming".)

       The statics installs an `as` name in the THEOREM namespace, so a
       citation of it resolves while the user types rather than only when
       the big-step checker runs. An `induction`'s `case_eq` has no
       statics-side model — the checker owns it — which is precisely why
       naming a split is what makes it visible here. */
    expects_no_mark(
      "an induction `as` name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => axiom h at 0 on n end | false => ? end in t|},
      is_free_hyp,
    ),
    /* ...and it is visible in EVERY case, not just the first. */
    expects_no_mark(
      "an induction `as` name is visible in every case",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => ? | false => axiom h at 0 on n end end in t|},
      is_free_hyp,
    ),
    /* An `as` name is scoped to its own form. */
    expects_mark(
      "an induction `as` name does not escape its form",
      {|theorem t = forall n -> n == n proof induction n > 0 as h | true => ? | false => ? end; axiom h at 0 on n end in t|},
      is_free_hyp,
    ),
    /* A DUPLICATE `as` name shadows rather than erroring: the inner
       binding hides the outer, and the citation still resolves. */
    expects_no_mark(
      "a duplicate `as` name shadows without erroring",
      {|theorem t = forall n -> forall m -> n == n proof induction n > 0 as h | true => induction m > 0 as h | true => axiom h at 0 on m end | false => ? end | false => ? end in t|},
      is_free_hyp,
    ),
    /* `assume <e> as <h>` likewise. */
    expects_no_mark(
      "an assume `as` name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof assume n == 1 as h => axiom h at 0 on n end in t|},
      is_free_hyp,
    ),
    /* An `as`/`alias` name binds in the THEOREM namespace ONLY — it is
       not a variable of the program, exactly like the auto-named ones. */
    expects_mark(
      "an `as` name is not an expression variable",
      {|theorem t = 1 == 1 proof assume 2 == 2 as h => ? in h|},
      is_free_var,
    ),
    /* `alias` re-binds an in-scope fact under a second name, and the
       body sees it. */
    expects_no_mark(
      "an alias name is a known hypothesis at edit time",
      {|theorem t = forall n -> n == n proof assume n == 1 => alias h = assume => axiom h at 0 on n end in t|},
      is_free_hyp,
    ),
    /* The alias's FACT slot is a fact reference, not a program variable:
       a bare name there must not read as free. (Body is `0`, not `t` —
       a theorem's own name IS free in expression position, by the
       namespace separation tested above, and would mask this.) */
    expects_no_mark(
      "an alias's fact slot resolves a hypothesis name",
      {|theorem t = forall n -> n == n proof assume n == 1 => alias h = assume => ? in 0|},
      is_free_var,
    ),
    /* ...and aliasing a name that is not a fact does mark. */
    expects_mark(
      "aliasing an unknown name marks at edit time",
      {|theorem t = forall n -> n == n proof alias h = nosuchfact => ? in t|},
      is_free_hyp,
    ),
  ],
);
