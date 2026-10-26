open Alcotest;
open Language;

/* Reset-list-sum-int-library: the Int sum/product list library
 * (test/libraries/list-sum-int) through parser, statics, evaluator and
 * the existing proof checker. Runtime values are exact (closed lists:
 * empty, singleton, several elements, negatives, a zero factor) and
 * agree with fold_left on closed lists. Proofs use existing steps only
 * (eval, refl_eq, rewrite, assume); the universal append law is pinned
 * Incomplete rather than manufactured. Programs end `in 0`. */

let parse = Test_Evaluator_Prelude.parse_exp;
let pr = Test_ProofMap.print_exp;
let lib = ListSumIntLibrarySource.definitions;

/* The value of a program's tail. */
let value = (src: string): string => {
  let (statics, elab) = Test_ProofMap.statics_and_elab(parse(src));
  let (result, _) = Evaluator.evaluate(~statics, ~env=Builtins.env_init, elab);
  pr(result);
};

let static_errors = (src: string): int => {
  let (statics, _) = Test_ProofMap.statics_and_elab(parse(src));
  Statics.Map.error_ids(statics) |> List.length;
};

/* Each closed fact holds: its value, with the library in scope, is true. */
let holds = (fact: string) => {
  let src = lib ++ fact;
  check(int, fact ++ ": static-valid", 0, static_errors(src));
  check(string, fact, "true", value(src));
};

let value_tests = [
  test_case("sum_int: empty, singleton, several, negatives", `Quick, () => {
    check(string, "sum_int([1, 2, 3]) evaluates to 6", "6", value(lib ++ "sum_int([1, 2, 3])"));
    List.iter(
      holds,
      [
        "sum_int([]) == 0",
        "sum_int([7]) == 7",
        "sum_int([1, 2, 3]) == 6",
        "sum_int([-4, 1, -2]) == -5",
        "sum_int([5, -5]) == 0",
      ],
    );
  }),
  test_case("prod_int: empty, singleton, several, negatives, zero", `Quick, () => {
    check(string, "prod_int([2, 3, 4]) evaluates to 24", "24", value(lib ++ "prod_int([2, 3, 4])"));
    List.iter(
      holds,
      [
        "prod_int([]) == 1",
        "prod_int([5]) == 5",
        "prod_int([2, 3, 4]) == 24",
        "prod_int([-2, 3]) == -6",
        "prod_int([-2, -3]) == 6",
        "prod_int([3, 0, 9]) == 0",
      ],
    );
  }),
  test_case("closed lists: agree with fold_left", `Quick, () =>
    List.iter(
      holds,
      [
        "sum_int([]) == fold_left([], fun (a, x) -> a + x, 0)",
        "sum_int([1, 2, 3]) == fold_left([1, 2, 3], fun (a, x) -> a + x, 0)",
        "sum_int([-4, 1, -2]) == fold_left([-4, 1, -2], fun (a, x) -> a + x, 0)",
        "prod_int([]) == fold_left([], fun (a, x) -> a * x, 1)",
        "prod_int([2, 3, 4]) == fold_left([2, 3, 4], fun (a, x) -> a * x, 1)",
        "prod_int([-2, 3]) == fold_left([-2, 3], fun (a, x) -> a * x, 1)",
      ],
    )
  ),
];

/* --- Proofs: one program, every theorem looked up by name ------------- */

let theorems = {|theorem sum_nil = sum_int([]) == 0 proof eval sum_int([]) at 0 end; axiom refl_eq at 0 on 0 == 0 end in
theorem prod_nil = prod_int([]) == 1 proof eval prod_int([]) at 0 end; axiom refl_eq at 0 on 1 == 1 end in
theorem sum_cons = forall h: Int -> forall t: [Int] -> sum_int(h :: t) == h + sum_int(t) proof eval sum_int(h :: t) at 0 end; axiom refl_eq at 0 on h + sum_int(t) == h + sum_int(t) end in
theorem prod_cons = forall h: Int -> forall t: [Int] -> prod_int(h :: t) == h * prod_int(t) proof eval prod_int(h :: t) at 0 end; axiom refl_eq at 0 on h * prod_int(t) == h * prod_int(t) end in
theorem sum_single = forall x: Int -> sum_int([x]) == x proof eval sum_int([x]) at 0 end; eval sum_int([]) at 0 end; rewrite x + 0 with x at 0 end; axiom refl_eq at 0 on x == x end in
theorem prod_single = forall x: Int -> prod_int([x]) == x proof eval prod_int([x]) at 0 end; eval prod_int([]) at 0 end; rewrite x * 1 with x at 0 end; axiom refl_eq at 0 on x == x end in
theorem inst_sum = 1 == 1 proof assume sum_int([-4, 1, -2]) == -5 => axiom refl_eq at 0 on 1 == 1 end in
theorem inst_prod = 1 == 1 proof assume prod_int([2, 3, 4]) == 24 => axiom refl_eq at 0 on 1 == 1 end in
theorem sum_append = forall xs: [Int] -> forall ys: [Int] -> sum_int(xs @ ys) == sum_int(xs) + sum_int(ys) proof ? in
0|};

let program = lib ++ theorems;

let checked = lazy(Test_ProofMap.eval_with_proof(parse(program)));

let proof_named = (name: string): (ProofMap.t, Proof.t) => {
  let (state, statics, _) = Lazy.force(checked);
  let pm = EvaluatorState.get_proof_map(state);
  switch (
    EvaluatorState.get_theorems(state)
    |> List.find_opt(((_, n, _, _)) => n == name)
  ) {
  | None => Alcotest.fail("no theorem " ++ name)
  | Some((id, _, _, _)) =>
    switch (Statics.Map.lookup_exp(id, statics)) {
    | Some({user_term, _}) =>
      switch (Exp.term_of(user_term)) {
      | Theorem(_, _, proof, _) => (pm, proof)
      | _ => Alcotest.fail(name ++ ": not a theorem node")
      }
    | None => Alcotest.fail(name ++ ": no statics for theorem")
    }
  };
};

let status = name => {
  let (pm, proof) = proof_named(name);
  ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof));
};

let proven = name => check(string, name ++ " full status", "Proven", status(name));

let proof_tests = [
  test_case("the library program is static-valid", `Quick, () =>
    check(int, "static errors", 0, static_errors(program))
  ),
  test_case("defining equations: Proven by eval + refl_eq", `Quick, () =>
    List.iter(proven, ["sum_nil", "prod_nil", "sum_cons", "prod_cons"])
  ),
  test_case("singletons: Proven by eval + the existing rewrite", `Quick, () =>
    List.iter(proven, ["sum_single", "prod_single"])
  ),
  test_case("closed instances: obligation discharged by evaluation", `Quick, () =>
    List.iter(
      name => {
        let (pm, proof) = proof_named(name);
        let obs = ProofMap.obligations_of_proof(pm, proof);
        check(
          list(string),
          name ++ " obligations",
          ["Evaluated"],
          List.map((ob: Obligation.t) => Obligation.show_discharge(ob.discharge), obs),
        );
        proven(name);
      },
      ["inst_sum", "inst_prod"],
    )
  ),
  test_case("frontier: the universal append law is not Proven (pinned hole)", `Quick, () =>
    check(string, "sum_append full status", "Incomplete", status("sum_append"))
  ),
];

let tests = ("ListSumIntLibrary", value_tests @ proof_tests);
