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
      {|theorem t = true proof forall x => axiom refl_eq at 0 on x end in t|},
      is_free_var,
    ),
    /* The theorem introduces the goal's outer `forall`-bound variables
       into the proof scope, so referencing `x` in the proof of
       `forall x -> x == x` is not a free variable. */
    expects_no_mark(
      "theorem goal forall var is visible in proof",
      {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in t|},
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
      {|theorem t = forall x where x == 1 -> x == 1 proof ? in t|},
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
      {|theorem t = forall x where x == 1 -> x == 1 proof axiom where at 0 on x end in t|},
      is_free_hyp,
    ),
  ],
);
