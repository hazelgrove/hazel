open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* Totality of functions (see the Totality.re header).
 *
 * "Total" means one thing: you may evaluate it, and if it is a function
 * you may apply it on its type's domain. There is no "total as a value
 * but possibly looping as a function".
 *
 * So a quantified binder of arrow type is a total map: `forall f -> ...`
 * licenses `f(x)` and `fold_left(xs, f, acc)`. What is refused is
 * instantiating such a binder at a looping closure, and any term that
 * reaches a non-structural fixpoint at all, including a bare `omega`,
 * since a looping map is not a total value.
 *
 * Totality is not definedness: `1/0` and `head([])` terminate (in
 * `err`), so they stay DomainConditions obligations. `inv = fun x ->
 * 1/x` is a total map here. */

let marks_of = (pm: ProofMap.t, proof: Proof.t): string =>
  switch (find_marked_sub(pm, proof)) {
  | Some((_, marks)) => String.concat("; ", List.map(ProofMark.show, marks))
  | None => "<no marks>"
  };

/* Passes the gate silently: no divergence mark, no obligations, and the
 * theorem is fully Proven. */
let expect_pass = (src: string): unit => {
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "no PossiblyDivergentInstantiation mark, got: " ++ marks_of(pm, proof),
    false,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(Alcotest.int, "zero obligations", 0, List.length(obs));
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProofMap.Proven => ()
  | other =>
    Alcotest.fail(
      "expected fully Proven, got: "
      ++ ProofMap.show_full_status(other)
      ++ "\nmarks: "
      ++ marks_of(pm, proof),
    )
  };
};

/* Refused: divergence mark, no obligations (divergence is never an
 * obligation), not proven. */
let expect_refused = (src: string): unit => {
  let (pm, proof, obs) = obligations(src);
  Alcotest.check(
    Alcotest.bool,
    "PossiblyDivergentInstantiation mark is emitted, marks: "
    ++ marks_of(pm, proof),
    true,
    has_mark_kind(pm, proof, is_divergent_instantiation),
  );
  Alcotest.check(
    Alcotest.int,
    "a refused step incurs no obligations",
    0,
    List.length(obs),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the refused proof is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* --- An abstract function is applicable ------------------------------ */

/* `forall f -> forall x -> f(x) == f(x)`: neither the arrow-typed `f`
 * nor `f(x)`, which has no visible definition, is refused. Both binders
 * are annotated so this tests the divergence gate alone (an unannotated
 * induction scrutinee is gated separately). */
let test_abstract_function_applied = () =>
  expect_pass(
    {|theorem t = forall f: (Int -> Int) -> forall x: Int -> f(x) == f(x) proof from f(x) == f(x) do rewrite refl end end in t|},
  );

/* --- Higher-order builtins -------------------------------------------- */

/* A concrete function through `fold_left`. `fold_left` is itself a
 * (structural) Hazel fixpoint whose body applies its own `f`
 * parameter, so this exercises the "parameters of a walked body are
 * assumed total" rule even with no abstract function in sight. */
let test_fold_left_concrete_function = () =>
  expect_pass(
    {|let add1 = fun (acc, _) -> acc + 1 in theorem t = forall xs: [Int] -> forall acc: Int -> fold_left(xs, add1, acc) == fold_left(xs, add1, acc) proof from fold_left(xs, add1, acc) == fold_left(xs, add1, acc) do rewrite refl end end in t|},
  );

/* An abstract `f` through `fold_left`. Walking
 * `fold_left`'s body reaches `f(acc, x)` where `f` is the builtin's own
 * parameter, standing for the quantified `f` we passed in — a total map,
 * so the application is licensed. */
/* TODO: the defining equation
 * `fold_left(x :: xs, f, acc) == fold_left(xs, f, f(acc, x))` is not
 * derivable: `eval fold_left(x :: xs, f, acc) at 0` inlines the
 * builtin's fixpoint body but doesn't step the `case` on a symbolic
 * `x :: xs`, so the step lands on an unreduced beta-redex and the
 * following `refl` reports `PatternNotFound`. That is a limit of
 * `step`, not of totality, so this file tests `E == E` instead. */
let test_fold_left_abstract_function = () =>
  expect_pass(
    {|theorem t = forall f: ((Int, Int) -> Int) -> forall x: Int -> forall xs: [Int] -> forall acc: Int -> fold_left(x :: xs, f, acc) == fold_left(x :: xs, f, acc) proof from fold_left(x :: xs, f, acc) == fold_left(x :: xs, f, acc) do rewrite refl end end in t|},
  );

/* --- Totality is not definedness -------------------------------------- */

/* `inv` divides. Division terminates (in `err` at 0), so `inv` is a
 * total map and `inv == inv` needs no funext and no divergence mark.
 * The `y != 0` side of it is DomainConditions' business, and there is
 * no division to scan in `inv == inv` itself. */
let test_inv_refl_no_divergence = () =>
  expect_pass(
    {|let inv = fun x -> 1 / x in theorem t = inv == inv proof from inv == inv do rewrite refl end end in t|},
  );

/* The plainest first-order instantiation passes. */
let test_first_order_var_refl = () =>
  expect_pass(
    {|theorem t = forall x -> x == x proof from x == x do rewrite refl end end in t|},
  );

/* --- Refusals --------------------------------------------------------- */

/* Non-structural (arithmetic) recursion: `n - 1` is not a subterm. */
let test_nonstructural_recursion_refused = () =>
  expect_refused(
    {|let f = fun n -> if n > 0 then f(n - 1) else 0 in theorem t = f(1) == f(1) proof from f(1) == f(1) do rewrite refl end end in t|},
  );

/* `omega` as a value: a looping map is not total, so `omega == omega`
 * is refused even though nothing is applied. */
let test_omega_as_value_refused = () =>
  expect_refused(
    {|let omega = fun x -> omega(x) in theorem t = omega == omega proof from omega == omega do rewrite refl end end in t|},
  );

/* A lambda whose body loops is not a total value either: Totality
 * looks at lambda bodies. */
let test_lambda_with_divergent_body_refused = () =>
  expect_refused(
    {|let omega = fun x -> omega(x) in theorem t = (fun y -> omega(y)) == (fun y -> omega(y)) proof from (fun y -> omega(y)) == (fun y -> omega(y)) do rewrite refl end end in t|},
  );

/* --- Instantiating a quantified function at a diverging function ------ */

/* The quantifier ranges over total maps, so the obligation to be total
 * lands here: `lem`'s `f` may not be instantiated at `omega`. */
let omega_lem =
  "let omega = fun x -> omega(x) in "
  /* A theorem statement's outer binders are auto-introduced
   * (ProofCheck.peel_stmt_binders), so no explicit `forall f =>` step. */
  ++ "theorem lem = forall f: (Int -> Int) -> f(0) == f(0) proof from f(0) == f(0) do rewrite refl end end in ";

let test_lemma_is_proven = () => {
  let (state, _, elab) = omega_lem ++ "lem" |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of_named("lem", elab);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | ProofMap.Proven => ()
  | other =>
    Alcotest.fail(
      "the quantified-function lemma should be Proven, got: "
      ++ ProofMap.show_full_status(other)
      ++ "\nmarks: "
      ++ marks_of(pm, proof),
    )
  };
};

/* Mirrors Test_Instantiation's `with n = bad(1)` fixture, with the
 * instantiated binder function-typed: `h` appears only in the
 * antecedent, so the target `inc(2)` leaves it underdetermined and
 * `with h = omega` is what supplies it — straight into the gate.
 * (The lemma itself is a hole here; the Proven quantified-function
 * lemma is covered by `test_lemma_is_proven` above.) */
let test_instantiation_at_omega_refused = () => {
  let src =
    "let omega = fun x -> omega(x) in "
    ++ "let inc = fun x -> x + 1 in "
    ++ "theorem lem = forall h: (Int -> Int) -> h(1) == 1 ==> inc(2) == 3 proof ? in "
    ++ "theorem g = inc(2) == 3 proof from inc(2) do rewrite lem with h = omega end end in g";
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let proof = proof_of_named("g", elab);
  if (!has_mark_kind(pm, proof, is_divergent_instantiation)) {
    Alcotest.fail(
      "instantiating a quantified function at `omega` should be refused\n"
      ++ dump(pm, proof),
    );
  };
};

/* The same with no annotations: this doesn't depend on the statics
   having resolved the binder's type. */
let test_abstract_function_applied_unannotated = () =>
  expect_pass(
    {|theorem t = forall f -> forall x -> f(x) == f(x) proof from f(x) == f(x) do rewrite refl end end in t|},
  );

let tests = (
  "Evaluator.FunctionTotality",
  [
    test_case(
      "an abstract quantified function is applicable",
      `Quick,
      test_abstract_function_applied,
    ),
    test_case(
      "an unannotated abstract function is applicable",
      `Quick,
      test_abstract_function_applied_unannotated,
    ),
    test_case(
      "fold_left with a concrete function passes",
      `Quick,
      test_fold_left_concrete_function,
    ),
    test_case(
      "fold_left with an abstract function passes",
      `Quick,
      test_fold_left_abstract_function,
    ),
    test_case(
      "inv == inv needs no divergence mark",
      `Quick,
      test_inv_refl_no_divergence,
    ),
    test_case(
      "first-order forall x -> x == x still passes",
      `Quick,
      test_first_order_var_refl,
    ),
    test_case(
      "non-structural recursion is still refused",
      `Quick,
      test_nonstructural_recursion_refused,
    ),
    test_case(
      "omega as a value is refused",
      `Quick,
      test_omega_as_value_refused,
    ),
    test_case(
      "a lambda with a divergent body is refused",
      `Quick,
      test_lambda_with_divergent_body_refused,
    ),
    test_case(
      "the quantified-function lemma is Proven",
      `Quick,
      test_lemma_is_proven,
    ),
    test_case(
      "instantiating at omega is refused",
      `Quick,
      test_instantiation_at_omega_refused,
    ),
  ],
);
