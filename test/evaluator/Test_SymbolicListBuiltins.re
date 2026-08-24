open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Symbolic list operators: `@` steps on CONSTRUCTOR form.
 *
 * Before this change, `ListConcat` in Transition.re required BOTH sides to
 * unbox as a `ListLit` (a known-length literal). A quantified `m: [Int]` is
 * a `Var`, which `Unboxing` reports as `IndetMatch`, so `[] @ m` and
 * `(y :: l) @ m` were stuck (`NothingToStep`) inside a proof `eval`. That is
 * why append proofs had to lean on hole lemmas.
 *
 * `@` is a PRIMITIVE with defining equations, exactly like the `case` a
 * user-defined `append` would be written with:
 *
 *     [] @ ys        ==  ys
 *     (x :: xs) @ ys ==  x :: (xs @ ys)
 *
 * Those two equations now fire whenever the LEFT side is in constructor
 * form, with the right side merely final. Deliberately NOT added:
 *   - the right unit `xs @ [] == xs`, which needs induction; and
 *   - any rule for a left side that is a bare variable or hole.
 *
 * Reuses the Test_ProofMap / Test_Funext harness. */

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
    | ProofMark.NothingToStep(_) => true
    | _ => false,
  );
};

/* --- 1. The two defining equations of `@` ----------------------------- */

/* `[] @ m == m` with `m` quantified. The left side is the empty literal,
 * the right side is a variable that will never become a literal: this is
 * the case that motivated the change. */
let test_append_nil_left = () =>
  check_status(
    "`[] @ m` steps to `m` with m quantified",
    ProofMap.Proven,
    {|theorem t = forall m: [Int] -> [] @ m == m proof eval [] @ m at 0 end; axiom refl_eq at 0 on m == m end in t|},
  );

/* `(y :: l) @ m == y :: (l @ m)`: BOTH the cons tail and the right side are
 * quantified variables, so nothing here can flatten to a literal. One step
 * peels the cons. */
let test_append_cons_left = () =>
  check_status(
    "`(y :: l) @ m` peels to `y :: (l @ m)` with l, m quantified",
    ProofMap.Proven,
    {|theorem t = forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m) proof eval (y :: l) @ m at 0 end; axiom refl_eq at 0 on y :: (l @ m) == y :: (l @ m) end in t|},
  );

/* --- 2. The ground fast path is unchanged ---------------------------- */

/* When both sides ARE literals, `@` still concatenates in a single step to
 * one literal rather than peeling one cons at a time. Existing eval and
 * stepper tests depend on this shape. */
let test_ground_concat_still_one_step = () =>
  check_status(
    "`[1, 2] @ [3]` still concatenates to a literal",
    ProofMap.Proven,
    {|theorem t = [1, 2] @ [3] == [1, 2, 3] proof eval [1, 2] @ [3] at 0 end; axiom refl_eq at 0 on [1, 2, 3] == [1, 2, 3] end in t|},
  );

/* --- 3. What must still be stuck ------------------------------------- */

/* A bare variable on the LEFT matches neither defining equation: no rule
 * applies, so `m @ n` is correctly stuck. (Closing `m @ n == m @ n` by
 * reflexivity is a separate, totality-flavoured question.) */
let test_var_left_is_stuck = () =>
  Alcotest.check(
    Alcotest.bool,
    "`m @ n` with both sides variables does not step",
    true,
    nothing_to_step(
      {|theorem t = forall m: [Int] -> forall n: [Int] -> m @ n == m @ n proof eval m @ n at 0 end; ? in t|},
    ),
  );

/* The right unit is deliberately NOT a primitive step: `m @ []` needs
 * induction on `m`. If this ever starts stepping, `@` has grown a rule
 * that the small-step semantics cannot justify. */
let test_right_unit_is_stuck = () =>
  Alcotest.check(
    Alcotest.bool,
    "`m @ []` does not step (the right unit needs induction)",
    true,
    nothing_to_step(
      {|theorem t = forall m: [Int] -> m @ [] == m proof eval m @ [] at 0 end; ? in t|},
    ),
  );

/* --- 4. `case`-defined list operations, for comparison ---------------- */

/* A user-defined `case` on a cons already matched constructor form before
 * this change; pinned here as the reference behaviour `@` now shares. */
let test_length_cons_proves = () =>
  check_status(
    "a `case`-defined length proves its cons equation",
    ProofMap.Proven,
    {|let len = fun l -> case l | [] => 0 | h :: t => 1 + len(t) end in theorem t = forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t) proof eval len(h :: t) at 0 end; axiom refl_eq at 0 on 1 + len(t) == 1 + len(t) end in t|},
  );

/* --- 5. The `append` builtin inherits the fix --------------------------
 *
 * `append` is literally `fun (xs, ys) -> xs @ ys` (BuiltinsList.re), so it
 * needs one extra `eval` to unfold the builtin and then rides the same
 * ListConcat equations. */

let test_append_nil_inherits = () =>
  check_status(
    "`append([], m)` unfolds to `[] @ m` and then to `m`",
    ProofMap.Proven,
    {|theorem t = forall m: [Int] -> append([], m) == m proof eval append([], m) at 0 end; eval $e at 0 end; axiom refl_eq at 0 on m == m end in t|},
  );

let test_append_cons_inherits = () =>
  check_status(
    "`append(y :: l, m)` unfolds and peels to `y :: (l @ m)`",
    ProofMap.Proven,
    {|theorem t = forall y: Int -> forall l: [Int] -> forall m: [Int] -> append(y :: l, m) == y :: (l @ m) proof eval append(y :: l, m) at 0 end; eval $e at 0 end; axiom refl_eq at 0 on y :: (l @ m) == y :: (l @ m) end in t|},
  );

/* --- 6. `fold_left`: steps, but does not REFOLD ------------------------
 *
 * `fold_left` is a HazelFn `case`, so it already matched constructor form
 * before this change and an `eval` on a cons DOES step (asserted below).
 * What it does not do is refold: the residual is the fixpoint's body
 * inlined and applied, not the surface term `fold_left(t, f, a + h)`, so
 * the defining equation does not close by reflexivity. That is a
 * presentation/refolding question about `case`-defined builtins, entirely
 * separate from `@`; pinned here as Incomplete so it is visible. */

let fold_left_cons_src = {|theorem t = forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, fun (acc, x) -> acc + x, a) == fold_left(t, fun (acc, x) -> acc + x, a + h) proof eval fold_left(h :: t, fun (acc, x) -> acc + x, a) at 0 end; axiom refl_eq at 0 on fold_left(t, fun (acc, x) -> acc + x, a + h) == fold_left(t, fun (acc, x) -> acc + x, a + h) end in t|};

let test_fold_left_cons_steps = () =>
  Alcotest.check(
    Alcotest.bool,
    "`fold_left` on a cons DOES step (it is a `case`, not a ListLit unbox)",
    false,
    nothing_to_step(fold_left_cons_src),
  );

let test_fold_left_cons_does_not_refold = () =>
  check_status(
    "pin: fold_left's cons equation does not close, because eval does not refold",
    ProofMap.Incomplete,
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
      "ground `[1, 2] @ [3]` still concatenates to a literal",
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
      "the `append` builtin inherits: `append([], m) == m`",
      `Quick,
      test_append_nil_inherits,
    ),
    test_case(
      "the `append` builtin inherits: `append(y :: l, m) == y :: (l @ m)`",
      `Quick,
      test_append_cons_inherits,
    ),
    test_case(
      "`fold_left` on a cons does step",
      `Quick,
      test_fold_left_cons_steps,
    ),
    test_case(
      "pin: fold_left's cons equation does not refold, so stays Incomplete",
      `Quick,
      test_fold_left_cons_does_not_refold,
    ),
  ],
);
