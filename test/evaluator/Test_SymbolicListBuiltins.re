open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Symbolic list operators: `@` steps when its left side is in constructor
 * form.
 *
 * A quantified `m: [Int]` is a `Var`, which never unboxes as a list
 * literal, so if `@` needed both sides to be literals, `[] @ m` and
 * `(y :: l) @ m` would be stuck inside a proof `eval`. Instead `@` has
 * defining equations, like the `case` a user-defined `append` would use:
 *
 *     [] @ ys        ==  ys
 *     (x :: xs) @ ys ==  x :: (xs @ ys)
 *
 * They fire whenever the left side is in constructor form and the right
 * side is final. There is no rule for
 *   - the right unit `xs @ [] == xs`, which needs induction; or
 *   - a left side that is a bare variable or hole.
 *
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let has_mark_kind = Test_ProofMap.has_mark_kind;

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

let nothing_to_step = (src: string): bool => {
  let (pm, proof) = run(src);
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.NothingToStep => true
    | _ => false,
  );
};

/* --- 1. The two defining equations of `@` ----------------------------- */

/* `[] @ m == m` with `m` quantified: the right side is a variable that
 * will never become a literal. */
let test_append_nil_left = () =>
  check_status(
    "`[] @ m` steps to `m` with m quantified",
    ProofMap.Proven,
    {|theorem t = forall m: [Int] -> [] @ m == m proof from [] @ m do eval end; from m == m do axiom refl_eq end end in t|},
  );

/* `(y :: l) @ m == y :: (l @ m)`: both the cons tail and the right side are
 * quantified variables, so nothing here can flatten to a literal. One step
 * peels the cons. */
let test_append_cons_left = () =>
  check_status(
    "`(y :: l) @ m` peels to `y :: (l @ m)` with l, m quantified",
    ProofMap.Proven,
    {|theorem t = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof from (y :: l) @ m do eval end; from y :: (l @ m) == y :: (l @ m) do axiom refl_eq end end in t|},
  );

/* --- 2. The ground fast path ----------------------------------------- */

/* When both sides are literals, `@` concatenates in a single step to one
 * literal rather than peeling one cons at a time. Eval and stepper tests
 * depend on this shape. */
let test_ground_concat_still_one_step = () =>
  check_status(
    "`[1, 2] @ [3]` concatenates to a literal",
    ProofMap.Proven,
    {|theorem t = [1, 2] @ [3] == [1, 2, 3] proof from [1, 2] @ [3] do eval end; from [1, 2, 3] == [1, 2, 3] do axiom refl_eq end end in t|},
  );

/* --- 3. What stays stuck --------------------------------------------- */

/* A bare variable on the left matches neither defining equation, so
 * `m @ n` is stuck. (Closing `m @ n == m @ n` by reflexivity is a
 * separate question, about totality.) */
let test_var_left_is_stuck = () =>
  Alcotest.check(
    Alcotest.bool,
    "`m @ n` with both sides variables does not step",
    true,
    nothing_to_step(
      {|theorem t = forall m: [Int] -> forall n: [Int] -> m @ n == m @ n proof from m @ n do eval end; ? in t|},
    ),
  );

/* The right unit is not a primitive step: `m @ []` needs induction on
 * `m`. If this steps, `@` has a rule the small-step semantics can't
 * justify. */
let test_right_unit_is_stuck = () =>
  Alcotest.check(
    Alcotest.bool,
    "`m @ []` does not step (the right unit needs induction)",
    true,
    nothing_to_step(
      {|theorem t = forall m: [Int] -> m @ [] == m proof from m @ [] do eval end; ? in t|},
    ),
  );

/* --- 4. `case`-defined list operations, for comparison ---------------- */

/* A user-defined `case` on a cons matches constructor form: the
 * behaviour `@` shares. */
let test_length_cons_proves = () =>
  check_status(
    "a `case`-defined length proves its cons equation",
    ProofMap.Proven,
    {|let len = fun l -> case l | [] => 0 | h :: t => 1 + len(t) end in theorem t = forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t) proof from len(h :: t) do eval end; from 1 + len(t) == 1 + len(t) do axiom refl_eq end end in t|},
  );

/* --- 5. The `append` builtin -----------------------------------------
 *
 * `append` is `fun (xs, ys) -> xs @ ys` (BuiltinsList.re), so it needs one
 * extra `eval` to unfold the builtin and then uses the same equations. */

let test_append_nil_inherits = () =>
  check_status(
    "`append([], m)` unfolds to `[] @ m` and then to `m`",
    ProofMap.Proven,
    {|theorem t = forall m: [Int] -> append([], m) == m proof from append([], m) do eval; eval end; axiom refl_eq end in t|},
  );

let test_append_cons_inherits = () =>
  check_status(
    "`append(y :: l, m)` unfolds and peels to `y :: (l @ m)`",
    ProofMap.Proven,
    {|theorem t = forall y: Int -> forall l: [Int] -> forall m: [Int] -> append(y :: l, m) == y :: (l @ m) proof from append(y :: l, m) do eval; eval end; axiom refl_eq end in t|},
  );

/* --- 6. `fold_left`: steps, and names its recursive call ---------------
 *
 * `fold_left` is a HazelFn `case`, so an `eval` on a cons steps, leaving
 * the recursive call as `fold_left(t, f, f(a, h))`. With the function
 * applied, the defining equation closes by reflexivity. */

let fold_left_cons_src = {|theorem t = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, fun (acc, x) -> acc + x, a) == fold_left(t, fun (acc, x) -> acc + x, a + h) proof from fold_left(h :: t, fun (acc, x) -> acc + x, a) do eval end; from (fun (acc, x) -> acc + x)((a, h)) do eval end; axiom refl_eq in t|};

let test_fold_left_cons_steps = () =>
  Alcotest.check(
    Alcotest.bool,
    "`fold_left` on a cons steps (it is a `case`, not a ListLit unbox)",
    false,
    nothing_to_step(fold_left_cons_src),
  );

let test_fold_left_cons_closes = () =>
  check_status(
    "fold_left's cons equation closes",
    ProofMap.Proven,
    fold_left_cons_src,
  );

let tests = (
  "Evaluator.SymbolicListBuiltins",
  [
    test_case(
      "`[] @ m == m` proves for quantified m",
      `Quick,
      test_append_nil_left,
    ),
    test_case(
      "`(y :: l) @ m == y :: (l @ m)` proves for quantified l, m",
      `Quick,
      test_append_cons_left,
    ),
    test_case(
      "ground `[1, 2] @ [3]` concatenates to a literal in one step",
      `Quick,
      test_ground_concat_still_one_step,
    ),
    test_case(
      "pin: `m @ n` with a variable on the left does not step",
      `Quick,
      test_var_left_is_stuck,
    ),
    test_case(
      "pin: the right unit `m @ []` does not step",
      `Quick,
      test_right_unit_is_stuck,
    ),
    test_case(
      "reference: a `case`-defined length proves its cons equation",
      `Quick,
      test_length_cons_proves,
    ),
    test_case(
      "the `append` builtin steps too: `append([], m) == m`",
      `Quick,
      test_append_nil_inherits,
    ),
    test_case(
      "the `append` builtin steps too: `append(y :: l, m) == y :: (l @ m)`",
      `Quick,
      test_append_cons_inherits,
    ),
    test_case(
      "`fold_left` on a cons does step",
      `Quick,
      test_fold_left_cons_steps,
    ),
    test_case(
      "fold_left's cons equation closes by eval and reflexivity",
      `Quick,
      test_fold_left_cons_closes,
    ),
  ],
);
