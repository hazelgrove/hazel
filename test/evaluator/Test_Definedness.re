open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 3a: definedness gates (docs/prover-obligations.md §4.1).
 *
 * Structural totality (Totality.re) refuses possibly-divergent
 * instantiations/scrutinees with marks; the domain scan
 * (DomainConditions.re) turns partial-primitive applications into
 * boolean obligations run through the ordinary discharge channels.
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let check_exp = Test_ProofMap.check_exp;

let obligations = (src: string): (ProofMap.t, Proof.t, list(Obligation.t)) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  (pm, proof, ProofMap.obligations_of_proof(pm, proof));
};

let check_count = (msg, expected, obs) =>
  Alcotest.check(Alcotest.int, msg, expected, List.length(obs));

let is_divergent_instantiation: ProofMark.t => bool =
  fun
  | ProofMark.PossiblyDivergentInstantiation(_) => true
  | _ => false;

let is_divergent_scrutinee: ProofMark.t => bool =
  fun
  | ProofMark.PossiblyDivergentScrutinee => true
  | _ => false;

let is_float_algebrite: ProofMark.t => bool =
  fun
  | ProofMark.FloatAlgebrite => true
  | _ => false;

/* --- Guarded division lemma, end to end ------------------------------ */

/* Using `inv : forall x where x != 0 -> x / x == 1` at a CLOSED nonzero
 * value: the instantiated guard `2 != 0` discharges by closed evaluation
 * (channel 2); the instantiation `x := 2` itself scans to nothing. */
let test_guarded_division_closed = () => {
  let src = {|theorem inv = forall x where x != 0 -> x / x == 1 proof ? in theorem u = 2 / 2 == 1 proof axiom inv at 0 on 2 / 2 end; axiom refl_eq at 0 on 1 == 1 end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  let obs = ProofMap.obligations_of_proof(pm, use_proof);
  check_count("exactly the guard obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is the instantiated guard", "2 != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged by closed evaluation",
      true,
      ob.discharge == Obligation.Evaluated,
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "use is fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, use_proof) == ProofMap.Proven,
  );
};

/* An OPEN instantiation containing division: `x := y / z` scans to the
 * denominator condition `z != 0`, which stays Pending (open goals are
 * never evaluated). */
let test_open_division_instantiation_pending = () => {
  let src = {|theorem u = forall y -> forall z -> y / z == y / z proof axiom refl_eq at 0 on y / z == y / z end in u|};
  let (pm, proof, obs) = obligations(src);
  check_count("one scanned obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is the denominator", "z != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the obligation is pending",
      true,
      Obligation.is_pending(ob),
    );
  | _ => ()
  };
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([_]) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with one obligation, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* A conditional rule instantiated at a term containing division emits
 * BOTH the instantiated guard and the scanned denominator condition. */
let test_conditional_rule_plus_domain_scan = () => {
  let src = {|theorem inv = forall x where x != 0 -> x / x == 1 proof ? in theorem u = forall a -> forall b -> (a / b) / (a / b) == 1 proof axiom inv at 0 on (a / b) / (a / b) end; axiom refl_eq at 0 on 1 == 1 end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let use_proof = proof_of_named("u", elab);
  let obs = ProofMap.obligations_of_proof(pm, use_proof);
  check_count("guard + scanned denominator", 2, obs);
  let has_goal = (expected: string) =>
    List.exists(
      (ob: Obligation.t) =>
        Exp.fast_equal(Test_ProofMap.elaborated_exp(expected), ob.goal),
      obs,
    );
  Alcotest.check(
    Alcotest.bool,
    "instantiated guard a / b != 0 is incurred",
    true,
    has_goal("a / b != 0"),
  );
  Alcotest.check(
    Alcotest.bool,
    "scanned denominator b != 0 is incurred",
    true,
    has_goal("b != 0"),
  );
};

/* --- Instantiation exemption (regression) ----------------------------- */

/* The common instantiations — a quantified variable, a sum of them —
 * emit NOTHING: no marks, no obligations. */
let test_instantiation_exemption_var = () => {
  let src = {|theorem t = forall x -> x == x proof axiom refl_eq at 0 on x == x end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := x emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let test_instantiation_exemption_total_ops = () => {
  let src = {|theorem t = forall a -> forall b -> a + b == a + b proof axiom refl_eq at 0 on a + b == a + b end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("x := a + b emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Totality refusal -------------------------------------------------- */

/* Instantiating at a call of a (general-)recursive function is refused:
 * divergence is never an obligation (§1.1). */
let test_divergent_instantiation_refused = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == f(1) proof axiom refl_eq at 0 on f(1) == f(1) end in t|};
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentInstantiation mark is emitted",
    true,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  check_count("a refused step incurs no obligations", 0, obs);
  Alcotest.check(
    Alcotest.option(bool),
    "the refused proof is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* The same recursive call inside an EVAL step is fine: eval steps are
 * denotation-preserving and carry no gates (§4.1). */
let test_recursive_call_in_eval_step_ungated = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == 0 proof eval f(1) at 0 end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("eval step incurs no obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark on an eval step",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
};

/* --- Split / induction gate -------------------------------------------- */

/* Ordinary structural induction on a bare quantified variable emits
 * nothing (regression). */
let test_induction_bare_variable_no_gate = () => {
  let src = {|theorem t = forall b -> b == b proof induction b | true => axiom refl_eq at 0 on true == true end | false => axiom refl_eq at 0 on false == false end end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("bare-variable induction emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no PossiblyDivergentScrutinee mark",
    false,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* A COMPUTED bool scrutinee (split) is domain-scanned: splitting on
 * `x / y == 1` emits `y != 0` on the induction node — path-insensitively,
 * before any branch is entered. */
let test_split_computed_scrutinee_emits_condition = () => {
  let src = {|theorem t = forall x -> forall y -> x / y == x / y proof induction x / y == 1 | true => ? | false => ? end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("split emits the denominator condition", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the condition is y != 0", "y != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the condition is pending",
      true,
      Obligation.is_pending(ob),
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "a total computed scrutinee is not refused",
    false,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* A computed scrutinee that calls a recursive function is refused. */
let test_split_divergent_scrutinee_refused = () => {
  let src = {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = forall x -> x == x proof induction f(1) == 0 | true => ? | false => ? end in t|};
  let (pm, proof, _obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentScrutinee mark is emitted",
    true,
    has_mark_kind(pm, proof, is_divergent_scrutinee),
  );
};

/* --- Algebrite gate ----------------------------------------------------- */

/* A rewrite whose sides contain division emits the denominator
 * obligation (deduplicated across the two sides). */
let test_algebrite_division_obligation = () => {
  let src = {|theorem t = forall a -> forall b -> a / b == a / b proof rewrite a / b with a / b at 0 end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("one deduplicated denominator obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is b != 0", "b != 0", ob.goal)
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "integer rewrite is not refused as float",
    false,
    has_mark_kind(pm, proof, is_float_algebrite),
  );
};

/* Float-typed Algebrite rewrites are refused: CAS field laws are false
 * for IEEE floats (§1.5). */
let test_algebrite_float_refused = () => {
  let src = {|theorem t = forall x -> x +. 1. ==. x +. 1. proof rewrite x +. 1. with 1. +. x at 0 end in t|};
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "FloatAlgebrite mark is emitted",
    true,
    has_mark_kind(pm, proof, is_float_algebrite),
  );
  check_count("a refused rewrite incurs no obligations", 0, obs);
};

/* --- Float totality regression (§1.5) ----------------------------------- */

/* Float division is IEEE-total: `1. /. 0.` is the VALUE infinity.
 * Instantiating at it emits NO obligation and NO refusal. */
let test_float_division_is_total = () => {
  let src = {|theorem t = 1. /. 0. == 1. /. 0. proof axiom refl_eq at 0 on 1. /. 0. == 1. /. 0. end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("float division emits zero obligations", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "no divergence mark",
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Eval steps stay free (regression) ---------------------------------- */

let test_eval_step_division_no_gate = () => {
  let src = {|theorem t = 4 / 2 == 2 proof eval 4 / 2 at 0 end; eval 2 == 2 at 0 end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("eval steps over division incur nothing", 0, obs);
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Scan coverage: builtins ------------------------------------------- */

/* `int_mod(a, b)` emits `b != 0`. */
let test_mod_builtin_condition = () => {
  let src = {|theorem t = forall a -> forall b -> int_mod(a, b) == int_mod(a, b) proof axiom refl_eq at 0 on int_mod(a, b) == int_mod(a, b) end in t|};
  let (_pm, _proof, obs) = obligations(src);
  check_count("one mod obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is b != 0", "b != 0", ob.goal)
  | _ => ()
  };
};

/* Int power with a variable exponent emits `y >= 0` (NegativeExponent
 * is the error; Nat power would be total). */
let test_power_exponent_condition = () => {
  let src = {|theorem t = forall x -> forall y -> x ** y == x ** y proof axiom refl_eq at 0 on x ** y == x ** y end in t|};
  let (_pm, _proof, obs) = obligations(src);
  check_count("one exponent obligation", 1, obs);
  switch (obs) {
  | [ob] => check_exp("the condition is y >= 0", "y >= 0", ob.goal)
  | _ => ()
  };
};

/* `int_of_float` at a closed finite float: the `is_finite` condition is
 * emitted and discharges by closed evaluation (channel 2). */
let test_of_float_is_finite_condition = () => {
  let src = {|theorem t = int_of_float(2.5) == int_of_float(2.5) proof axiom refl_eq at 0 on int_of_float(2.5) == int_of_float(2.5) end in t|};
  let (pm, proof, obs) = obligations(src);
  check_count("one is_finite obligation", 1, obs);
  Alcotest.check(
    Alcotest.bool,
    "discharged by closed evaluation",
    true,
    switch (obs) {
    | [ob] => ob.discharge == Obligation.Evaluated
    | _ => false
    },
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let tests = (
  "Evaluator.Definedness",
  [
    test_case(
      "guarded division lemma at a closed value (Evaluated)",
      `Quick,
      test_guarded_division_closed,
    ),
    test_case(
      "open instantiation with division stays Pending",
      `Quick,
      test_open_division_instantiation_pending,
    ),
    test_case(
      "conditional rule adds its guard on top of the domain scan",
      `Quick,
      test_conditional_rule_plus_domain_scan,
    ),
    test_case(
      "instantiation at a quantified variable emits nothing",
      `Quick,
      test_instantiation_exemption_var,
    ),
    test_case(
      "instantiation at a total-op composition emits nothing",
      `Quick,
      test_instantiation_exemption_total_ops,
    ),
    test_case(
      "recursive-call instantiation is refused",
      `Quick,
      test_divergent_instantiation_refused,
    ),
    test_case(
      "recursive call inside an eval step is ungated",
      `Quick,
      test_recursive_call_in_eval_step_ungated,
    ),
    test_case(
      "bare-variable induction emits no gate traffic",
      `Quick,
      test_induction_bare_variable_no_gate,
    ),
    test_case(
      "bool split on a computed scrutinee emits its domain condition",
      `Quick,
      test_split_computed_scrutinee_emits_condition,
    ),
    test_case(
      "bool split on a divergent scrutinee is refused",
      `Quick,
      test_split_divergent_scrutinee_refused,
    ),
    test_case(
      "algebrite rewrite emits denominator obligations",
      `Quick,
      test_algebrite_division_obligation,
    ),
    test_case(
      "float-typed algebrite rewrite is refused",
      `Quick,
      test_algebrite_float_refused,
    ),
    test_case(
      "float division is IEEE-total: no obligation, no refusal",
      `Quick,
      test_float_division_is_total,
    ),
    test_case(
      "eval steps over division stay free",
      `Quick,
      test_eval_step_division_no_gate,
    ),
    test_case("mod builtin emits b != 0", `Quick, test_mod_builtin_condition),
    test_case(
      "int power emits exponent >= 0",
      `Quick,
      test_power_exponent_condition,
    ),
    test_case(
      "int_of_float emits is_finite, closed-evaluated",
      `Quick,
      test_of_float_is_finite_condition,
    ),
  ],
);
