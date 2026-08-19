open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 3b: function contracts (docs/prover-obligations.md §2.2).
 *
 * `fun p where g -> e` (FunWhere) has two proof-layer effects and NO
 * dynamic effect:
 *   - definition-time discharge: the body's domain conditions are
 *     checked against the guard once, at the definition
 *     (ProofCheck.definition_obligations);
 *   - caller-vocabulary obligations: applying the function emits the
 *     instantiated contract `g[p := arg]`, never body internals
 *     (DomainConditions.scan).
 * Plus the closure-lemma library (§4.2 channel 3): guarded
 * equality-form axioms usable as ordinary conditional rewrite rules.
 *
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let check_exp = Test_ProofMap.check_exp;
let elaborated_exp = Test_ProofMap.elaborated_exp;

let run = (src: string): (ProofMap.t, Proof.t, list(Obligation.t)) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of(elab);
  (pm, proof, ProofMap.obligations_of_proof(pm, proof));
};

let check_count = (msg, expected, obs) =>
  Alcotest.check(Alcotest.int, msg, expected, List.length(obs));

/* Definition-time entries live at the FUNCTION's id — outside any proof
 * subtree — with the minimal non-proof shape (incoming/outgoing None,
 * no marks, only obligations). */
let definition_entries = (pm: ProofMap.t): list((Id.t, ProofMap.entry)) =>
  Id.Map.fold(
    (id, entry: ProofMap.entry, acc) =>
      switch (entry) {
      | {
          incoming: None,
          outgoing: None,
          marks: [],
          obligations: [_, ..._],
          _,
        } =>
        acc @ [(id, entry)]
      | _ => acc
      },
    pm,
    [],
  );

let is_remote: Obligation.discharge => bool =
  fun
  | Obligation.Remote(_) => true
  | _ => false;

/* --- Dynamics: the guard has no dynamic effect ------------------------ */

/* A FunWhere applies exactly like the corresponding Fun — including on
 * arguments OUTSIDE the contract (no runtime enforcement in v1,
 * Grammar.re). */
let test_funwhere_applies_like_fun = () => {
  parse_and_evaluate_test(
    ~msg="contract fun applies like fun",
    "50",
    "(fun x where x != 0 -> 100 / x)(2)",
  );
};

/* --- Caller-vocabulary contract obligations (§2.2) -------------------- */

/* Using `f(y)` where `f = fun x where x != 0 -> 100 / x` emits the
 * INSTANTIATED contract `y != 0` — caller vocabulary, not `x != 0` and
 * not conditions about `100 / x` — discharged Remote against the
 * theorem's where-binder. Zero pending anywhere (the definition's body
 * condition `x != 0` matches the guard and leaves no residue). */
let test_contract_emission_caller_vocabulary = () => {
  let src = {|let f = fun x where x != 0 -> 100 / x in theorem t = forall y where y != 0 -> f(y) == f(y) proof axiom refl_eq at 0 on f(y) == f(y) end in t|};
  let (pm, proof, obs) = run(src);
  check_count("exactly the contract obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is g[x := y]", "y != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the where-binder",
      true,
      is_remote(ob.discharge),
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
  check_count(
    "guarded definition leaves ZERO definition obligations",
    0,
    definition_entries(pm),
  );
};

/* A closed contract call `f(2)`: the instantiated contract `2 != 0`
 * discharges by closed evaluation (channel 2). */
let test_closed_contract_call_evaluated = () => {
  let src = {|let f = fun x where x != 0 -> 100 / x in theorem t = f(2) == f(2) proof axiom refl_eq at 0 on f(2) == f(2) end in t|};
  let (pm, proof, obs) = run(src);
  check_count("exactly the contract obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is g[x := 2]", "2 != 0", ob.goal);
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
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- Definition-time discharge (§2.2) --------------------------------- */

/* An UNGUARDED partial function used in a proof: the body condition
 * `x != 0` is recorded ONCE at the definition (Pending, origin = the
 * function's own id); calls do NOT re-emit body conditions. Two
 * theorems using it still yield one definition entry. */
let test_unguarded_definition_obligation_once = () => {
  let src = {|let g = fun x -> 100 / x in theorem t = forall y -> g(y) == g(y) proof axiom refl_eq at 0 on g(y) == g(y) end in theorem u = g(2) == g(2) proof axiom refl_eq at 0 on g(2) == g(2) end in u|};
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  /* Neither use re-emits the body condition. */
  check_count(
    "call sites emit nothing",
    0,
    ProofMap.obligations_of_proof(
      pm,
      Test_ProofMap.proof_of_named("t", elab),
    )
    @ ProofMap.obligations_of_proof(
        pm,
        Test_ProofMap.proof_of_named("u", elab),
      ),
  );
  /* Exactly one definition entry, with the body condition Pending. */
  switch (definition_entries(pm)) {
  | [(id, {obligations: [ob], _})] =>
    check_exp("the definition condition is x != 0", "x != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "the condition is Pending",
      true,
      Obligation.is_pending(ob),
    );
    Alcotest.check(
      Alcotest.bool,
      "origin is the definition's own id",
      true,
      ob.origin == id,
    );
  | entries =>
    Alcotest.fail(
      "expected exactly one single-obligation definition entry, got "
      ++ string_of_int(List.length(entries)),
    )
  };
};

/* The guarded twin: `fun x where x != 0 -> 100 / x` — the body scan's
 * `x != 0` matches the guard by fast_equal; ZERO definition
 * obligations. */
let test_guarded_definition_discharged = () => {
  let src = {|let f = fun x where x != 0 -> 100 / x in theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|};
  let (pm, proof, _) = run(src);
  check_count(
    "guard covers the body: zero definition obligations",
    0,
    definition_entries(pm),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* A conjunctive guard discharges per-conjunct: `x != 0 && y-independent
 * leftovers` — here `x != 0 && x != 1` covers the body's `x != 0` and
 * leaves nothing. */
let test_conjunctive_guard_discharges = () => {
  let src = {|let f = fun x where x != 0 && x != 1 -> 100 / x in theorem t = 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end in t|};
  let (pm, _, _) = run(src);
  check_count(
    "conjunct covers the body condition",
    0,
    definition_entries(pm),
  );
};

/* --- Closure-lemma library (§4.2 channel 3) --------------------------- */

/* `nonzero_mul` rewrites `a * b != 0` to `true` as an ordinary
 * conditional rule; its antecedents `a != 0` / `b != 0` are incurred
 * through the Phase-2 apply-with-obligation machinery and discharged
 * Remote against the where-binders. Proven, zero pending. */
let test_nonzero_mul_discharged_by_binders = () => {
  let src = {|theorem t = forall a where a != 0 -> forall b where b != 0 -> a * b != 0 proof axiom nonzero_mul at 0 on a * b != 0 end in t|};
  let (pm, proof, obs) = run(src);
  check_count("the two antecedent obligations", 2, obs);
  let has_goal = (expected: string) =>
    List.exists(
      (ob: Obligation.t) =>
        Exp.fast_equal(elaborated_exp(expected), ob.goal)
        && is_remote(ob.discharge),
      obs,
    );
  Alcotest.check(
    Alcotest.bool,
    "a != 0 incurred and discharged Remote",
    true,
    has_goal("a != 0"),
  );
  Alcotest.check(
    Alcotest.bool,
    "b != 0 incurred and discharged Remote",
    true,
    has_goal("b != 0"),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* The negative: without the where-binders nothing covers the
 * antecedents — ProvenModulo with the two Pending. */
let test_nonzero_mul_pending_without_binders = () => {
  let src = {|theorem t = forall a -> forall b -> a * b != 0 proof axiom nonzero_mul at 0 on a * b != 0 end in t|};
  let (pm, proof, obs) = run(src);
  check_count("the two antecedent obligations", 2, obs);
  Alcotest.check(
    Alcotest.bool,
    "both are Pending",
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProvenModulo([_, _]) => ()
  | other =>
    Alcotest.fail(
      "expected ProvenModulo with two pending, got: "
      ++ ProofMap.show_full_status(other),
    )
  };
};

/* Bridging lemma: `nonzero_of_pos` turns a `> 0` fact into the `!= 0`
 * an obligation needs — the §4.2 "nearest miss" bridge, today as a
 * manual axiom step rewriting the goal itself. */
let test_nonzero_of_pos_bridge = () => {
  let src = {|theorem t = forall a where a > 0 -> a != 0 proof axiom nonzero_of_pos at 0 on a != 0 end in t|};
  let (pm, proof, obs) = run(src);
  check_count("one antecedent obligation", 1, obs);
  switch (obs) {
  | [ob] =>
    check_exp("the antecedent is a > 0", "a > 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the where-binder",
      true,
      is_remote(ob.discharge),
    );
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* Closed antecedents just run (channel 2): `pos_mul` at literals. */
let test_pos_mul_closed_evaluated = () => {
  let src = {|theorem t = 2 * 3 > 0 proof axiom pos_mul at 0 on 2 * 3 > 0 end in t|};
  let (pm, proof, obs) = run(src);
  check_count("the two antecedent obligations", 2, obs);
  Alcotest.check(
    Alcotest.bool,
    "both discharged by closed evaluation",
    true,
    List.for_all(
      (ob: Obligation.t) => ob.discharge == Obligation.Evaluated,
      obs,
    ),
  );
  Alcotest.check(
    Alcotest.bool,
    "fully Proven",
    true,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

let tests = (
  "Evaluator.FunContracts",
  [
    test_case(
      "FunWhere applies like Fun (guard has no dynamic effect)",
      `Quick,
      test_funwhere_applies_like_fun,
    ),
    test_case(
      "contract use emits the instantiated guard, caller vocabulary",
      `Quick,
      test_contract_emission_caller_vocabulary,
    ),
    test_case(
      "closed contract call is Evaluated",
      `Quick,
      test_closed_contract_call_evaluated,
    ),
    test_case(
      "unguarded definition: body condition recorded once, Pending",
      `Quick,
      test_unguarded_definition_obligation_once,
    ),
    test_case(
      "guarded definition: body condition matches guard, zero leftovers",
      `Quick,
      test_guarded_definition_discharged,
    ),
    test_case(
      "conjunctive guard discharges per-conjunct",
      `Quick,
      test_conjunctive_guard_discharges,
    ),
    test_case(
      "nonzero_mul discharged against where-binders",
      `Quick,
      test_nonzero_mul_discharged_by_binders,
    ),
    test_case(
      "nonzero_mul pending without binders (ProvenModulo)",
      `Quick,
      test_nonzero_mul_pending_without_binders,
    ),
    test_case(
      "nonzero_of_pos bridges > 0 to != 0",
      `Quick,
      test_nonzero_of_pos_bridge,
    ),
    test_case(
      "pos_mul closed antecedents evaluate",
      `Quick,
      test_pos_mul_closed_evaluated,
    ),
  ],
);
