open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* `funext x => <proof>`: functional extensionality as a proof KEYWORD,
 * not an axiom. An axiom is a rewrite equality; stating funext as one
 * would have to quantify over functions at unknown/hole types. Funext is
 * an INTRO rule, the sibling of `forall x =>` / `assume`.
 *
 * Rule: with incoming goal `f == g` at an ARROW type, the body's incoming
 * goal is the pointwise `f(x) == g(x)` and `x` is bound at the domain
 * type. Outgoing is `true` when the body closes that; otherwise the outer
 * `f == g` passes through (same convention as Forall).
 *
 * Reuses the Test_ProofMap harness, like Test_Generalize. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let print_exp = Test_ProofMap.print_exp;

let run = (src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of(elab));
};

let full_status = (src: string): ProofMap.full_status => {
  let (pm, proof) = run(src);
  ProofMap.full_status_of_proof(pm, proof);
};

let check_status = (msg: string, expected: ProofMap.full_status, src: string) => {
  let actual = full_status(src);
  Alcotest.check(
    Alcotest.bool,
    msg ++ " (got: " ++ ProofMap.show_full_status(actual) ++ ")",
    true,
    actual == expected,
  );
};

/* The immediate body of a `funext` node — the sub-proof that sees the
 * pointwise goal. */
let funext_body = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Funext(_, body) => body
  | _ => Alcotest.fail("expected the proof to be a funext node")
  };

let incoming_of = (pm: ProofMap.t, p: Proof.t): Exp.t =>
  switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
  | Some({incoming: Some(e), _}) => e
  | _ => Alcotest.fail("no incoming goal recorded for the proof node")
  };

/* --- 1. Happy path ---------------------------------------------------- */

/* The commuting pointwise-as-functions theorem. `funext x` turns the
 * function equality into `(fun x -> x + 1)(x) == (fun x -> 1 + x)(x)`,
 * the rewrite commutes the left body, and refl closes it. Fully Proven. */
let test_happy_path_proven = () =>
  check_status(
    "pointwise commute proves as a function equality",
    ProofMap.Proven,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => rewrite x + 1 with 1 + x at 0 end; axiom refl_eq at 0 on 1 + x == 1 + x end in t|},
  );

/* The body's incoming goal is the pointwise one. When a side is a
 * LITERAL lambda, the application is a beta-redex whose contraction is
 * definitional, so the goal the body sees is the contracted `x + 1 == 1 + x`
 * — which is what makes a plain pointwise `rewrite` reach it. */
let test_body_goal_is_pointwise = () => {
  let src = {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => ? in t|};
  let (pm, proof) = run(src);
  let goal = incoming_of(pm, funext_body(proof));
  Test_ProofMap.check_exp(
    "the body sees the contracted pointwise goal (got: "
    ++ print_exp(goal)
    ++ ")",
    "x + 1 == 1 + x",
    goal,
  );
};

/* A NAMED function stays an application: unfolding a definition is
 * `eval`'s job, not funext's. This is the invariant that keeps
 * `refl_eq` on `inv(x) == inv(x)` a claim about `inv` applied to an
 * arbitrary argument rather than a claim about `inv`'s body. */
let test_named_function_stays_applied = () => {
  let src = {|let inv = fun x -> 1 / x in theorem t = inv == inv proof funext x => ? in t|};
  let (pm, proof) = run(src);
  let goal = incoming_of(pm, funext_body(proof));
  let is_ap = (e: Exp.t) =>
    switch (e |> Exp.term_of) {
    | Ap(_, _, _) => true
    | _ => false
    };
  switch (goal |> Exp.term_of) {
  | BinOp(Poly(Equals), l, r) =>
    Alcotest.check(
      Alcotest.bool,
      "both sides stay applications for a named function (got: "
      ++ print_exp(goal)
      ++ ")",
      true,
      is_ap(l) && is_ap(r),
    )
  | _ =>
    Alcotest.fail("expected an `==` body goal, got: " ++ print_exp(goal))
  };
};

/* And that pointwise goal closes by reflexivity WITHOUT ever applying
 * `inv` — correct, and deliberately distinct from proving
 * `inv == (fun x -> 1 / x)` (see the partiality test below). */
let test_named_function_refl_proves = () =>
  check_status(
    "`inv == inv` closes pointwise by refl without applying inv",
    ProofMap.Proven,
    {|let inv = fun x -> 1 / x in theorem t = inv == inv proof funext x => axiom refl_eq at 0 on inv(x) == inv(x) end in t|},
  );

/* --- 2. Control: refl at identical functions, WITHOUT funext ---------- */

/* `lxvmkkxt` deleted `Mark.CompareFun`, so `f == f` between two
 * syntactically identical lambdas already closes by reflexivity. Funext
 * must not have broken that path. */
let test_refl_identical_functions_no_funext = () =>
  check_status(
    "refl closes identical function literals without funext",
    ProofMap.Proven,
    {|theorem t = (fun x -> x + 1) == (fun x -> x + 1) proof axiom refl_eq at 0 on (fun x -> x + 1) == (fun x -> x + 1) end in t|},
  );

/* --- 3. Malformed: funext on a non-function equality ------------------ */

/* `1 == 1` is an equality, but not at an arrow type: there is no
 * pointwise goal to form. Mark and pass the goal through, the same
 * recovery `forall` uses for `ExpectedForallGoal`. */
let test_non_arrow_equality_marked = () => {
  let src = {|theorem t = 1 == 1 proof funext x => ? in t|};
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "ExpectedFunEqGoal is emitted on a non-arrow `==`",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.ExpectedFunEqGoal => true
      | _ => false,
    ),
  );
  Alcotest.check(
    Alcotest.bool,
    "a marked funext does not prove the theorem",
    false,
    ProofMap.full_status_of_proof(pm, proof) == ProofMap.Proven,
  );
};

/* --- 4. Control: the hole, with no funext ----------------------------- */

/* Pins the parent's behaviour: a bare hole against a function equality is
 * Incomplete. If this ever flips to Proven, function `==` has silently
 * started to COMPUTE, which is a different (and wrong) story from funext. */
let test_hole_without_funext_incomplete = () =>
  check_status(
    "a hole against a function equality stays Incomplete",
    ProofMap.Incomplete,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t|},
  );

/* A `funext` whose body is still a hole is Incomplete too — the form
 * itself proves nothing. */
let test_funext_hole_body_incomplete = () =>
  check_status(
    "funext with a hole body is Incomplete",
    ProofMap.Incomplete,
    {|theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof funext x => ? in t|},
  );

/* --- 5. Currying: one arrow per funext -------------------------------- */

/* `a -> b -> c` peels once to `f(x) == g(x)`, which is still an arrow; a
 * second `funext y =>` peels again. Status is pinned to what the checker
 * actually reports rather than asserted optimistically — see the module
 * docstring in the report. */
let test_two_arrow_nested_funext = () => {
  let src = {|theorem t = (fun x -> fun y -> x + y) == (fun x -> fun y -> y + x) proof funext x => funext y => rewrite x + y with y + x at 0 end; axiom refl_eq at 0 on y + x == y + x end in t|};
  let (pm, proof) = run(src);
  let status = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "two nested funext close the curried commute (got: "
    ++ ProofMap.show_full_status(status)
    ++ ")",
    true,
    status == ProofMap.Proven,
  );
};

/* The inner funext's goal is the twice-applied pointwise equality. */
let test_two_arrow_inner_goal = () => {
  let src = {|theorem t = (fun x -> fun y -> x + y) == (fun x -> fun y -> y + x) proof funext x => funext y => ? in t|};
  let (pm, proof) = run(src);
  let inner = funext_body(proof);
  let inner_goal = incoming_of(pm, funext_body(inner));
  Alcotest.check(
    Alcotest.bool,
    "the inner body goal is an equality (got: "
    ++ print_exp(inner_goal)
    ++ ")",
    true,
    switch (inner_goal |> Exp.term_of) {
    | BinOp(Poly(Equals), _, _) => true
    | _ => false
    },
  );
};

/* --- 6. Partiality: no automatic `where` on the body goal -------------- */

/* Funext deliberately does NOT wrap the body goal in a domain
 * restriction. Partiality shows up where it actually bites — when a later
 * step APPLIES `f(x)`. Proving `inv == (fun x -> 1 / x)` by evaluating the
 * application is therefore not a clean Proven; this pins whatever the
 * checker reports so the behaviour is inspectable rather than assumed. */
let test_partial_function_not_cleanly_proven = () => {
  let src = {|let inv = fun x -> 1 / x in theorem t = inv == (fun x -> 1 / x) proof funext x => axiom refl_eq at 0 on inv(x) == inv(x) end in t|};
  let (pm, proof) = run(src);
  let status = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    "a partial function equality is not a clean Proven (got: "
    ++ ProofMap.show_full_status(status)
    ++ ")",
    false,
    status == ProofMap.Proven,
  );
};

let tests = (
  "Funext",
  [
    test_case(
      "happy path: pointwise commute proves as `f == g`",
      `Quick,
      test_happy_path_proven,
    ),
    test_case(
      "the body's goal is the contracted pointwise equality",
      `Quick,
      test_body_goal_is_pointwise,
    ),
    test_case(
      "a named function stays applied in the pointwise goal",
      `Quick,
      test_named_function_stays_applied,
    ),
    test_case(
      "`inv == inv` closes pointwise by refl, without applying inv",
      `Quick,
      test_named_function_refl_proves,
    ),
    test_case(
      "control: refl still closes identical functions without funext",
      `Quick,
      test_refl_identical_functions_no_funext,
    ),
    test_case(
      "funext on a non-arrow `==` is marked ExpectedFunEqGoal",
      `Quick,
      test_non_arrow_equality_marked,
    ),
    test_case(
      "control: a hole against a function equality stays Incomplete",
      `Quick,
      test_hole_without_funext_incomplete,
    ),
    test_case(
      "funext with a hole body is Incomplete",
      `Quick,
      test_funext_hole_body_incomplete,
    ),
    test_case(
      "currying: two nested funext peel two arrows",
      `Quick,
      test_two_arrow_nested_funext,
    ),
    test_case(
      "currying: the inner body goal is an equality",
      `Quick,
      test_two_arrow_inner_goal,
    ),
    test_case(
      "partiality: no automatic `where`, so `inv == ...` is not clean",
      `Quick,
      test_partial_function_not_cleanly_proven,
    ),
  ],
);
