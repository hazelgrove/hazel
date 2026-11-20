open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* A rule cited at a term needs the term standard, as quantifiers range
 * over standard values. The standardness routine reduces that to the
 * term's domain conditions, or else leaves `!!e` to prove; nothing is
 * refused. A quantified binder is standard, a function-typed one
 * included, so it can be applied. A function that may not give a
 * standard result on some standard argument isn't standard: one that
 * loops, and one that divides by its argument. Each test cites
 * `refl_lemma`, since the built-in `refl` holds at every term. */

let expect_standard = (src: string): unit => {
  let (pm, proof, obs) = obligations_named("t", refl_lemma ++ src);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "no obligations",
    [],
    List.map(~f=show_obligation, obs),
  );
  check_proven("Proven", pm, proof);
};

let expect_unknown = (term: string, src: string): unit => {
  let (pm, proof, obs) = obligations_named("t", refl_lemma ++ src);
  check_mark_free("nothing is refused", pm, proof);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "the term's standardness is left to prove",
    ["!! " ++ term],
    standardness_goals(obs),
  );
};

/* --- A quantified function is applicable ----------------------------- */

let test_abstract_function_applied = () =>
  expect_standard(
    {|theorem t = forall f: (Int -> Int) -> forall x: Int -> f(x) == f(x) proof from f(x) == f(x) do rewrite same end end in t|},
  );

/* With no types, `x` isn't known to be of `f`'s domain. */
let test_abstract_function_unannotated = () =>
  expect_unknown(
    "f(x)",
    {|theorem t = forall f -> forall x -> f(x) == f(x) proof from f(x) == f(x) do rewrite same end end in t|},
  );

let test_fold_left_abstract_function = () =>
  expect_standard(
    {|theorem t = forall f: ((Int, Int) -> Int) -> forall xs: [Int] -> forall acc: Int -> fold_left(xs, f, acc) == fold_left(xs, f, acc) proof from fold_left(xs, f, acc) == fold_left(xs, f, acc) do rewrite same end end in t|},
  );

/* --- Functions that aren't standard ----------------------------------- */

/* `inv(0)` is an error, so `inv` isn't standard. */
let test_inv_unknown = () =>
  expect_unknown(
    "inv",
    {|let inv = fun (x: Int) -> 1 / x in theorem t = inv == inv proof from inv == inv do rewrite same end end in t|},
  );

let test_nonstructural_recursion_unknown = () =>
  expect_unknown(
    "f(1)",
    {|let f = fun n -> if n == 0 then 0 else f(n - 1) in theorem t = f(1) == f(1) proof from f(1) == f(1) do rewrite same end end in t|},
  );

let omega = "let omega = fun (x: Int) -> omega(x) in ";

let test_omega_unknown = () =>
  expect_unknown(
    "omega",
    omega
    ++ {|theorem t = omega == omega proof from omega == omega do rewrite same end end in t|},
  );

let test_lambda_with_divergent_body_unknown = () =>
  expect_unknown(
    "(fun (y:(Int)) -> omega(y))",
    omega
    ++ {|theorem t = (fun (y: Int) -> omega(y)) == (fun (y: Int) -> omega(y)) proof from (fun (y: Int) -> omega(y)) == (fun (y: Int) -> omega(y)) do rewrite same end end in t|},
  );

/* --- `refl` holds at every term ----------------------------------------- */

let test_refl_at_omega = () => {
  let (pm, proof, obs) =
    obligations(
      omega
      ++ {|theorem t = omega == omega proof from omega == omega do rewrite refl end end in t|},
    );
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "no obligations",
    [],
    List.map(~f=show_obligation, obs),
  );
  check_proven("Proven", pm, proof);
};

/* --- Instantiating a quantified function -------------------------------- */

let test_function_lemma_is_proven = () => {
  let (pm, proof) =
    run_named(
      "lem",
      omega
      ++ {|theorem lem = forall f: (Int -> Int) -> f(0) == f(0) proof from f(0) == f(0) do rewrite refl end end in lem|},
    );
  check_proven("the quantified-function lemma", pm, proof);
};

/* `with h = omega` supplies the binder the target leaves undetermined. */
let test_instantiation_at_omega_unknown = () => {
  let (pm, proof, obs) =
    obligations_named(
      "g",
      omega
      ++ "let inc = fun x -> x + 1 in "
      ++ "theorem lem = forall h: (Int -> Int) -> h(1) == 1 ==> inc(2) == 3 proof ? in "
      ++ "theorem g = inc(2) == 3 proof from inc(2) do rewrite lem with h = omega end end in g",
    );
  check_mark_free("nothing is refused", pm, proof);
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "omega's standardness is left to prove",
    ["!! omega"],
    standardness_goals(obs),
  );
};

/* --- Focusing under a binder of the goal ------------------------------- */

/* Rewriting under a lambda of the goal holds for the values its
 * variable takes there, which are standard when its type is data. */
let add_zero = "theorem add_zero = forall a: Int -> a + 0 == a proof ? in ";

let focused = (goal: string, selector: string): list(string) => {
  let (_, _, obs) =
    obligations_named(
      "t",
      add_zero
      ++ "theorem t = "
      ++ goal
      ++ " proof from "
      ++ selector
      ++ " do rewrite add_zero end end in t",
    );
  standardness_goals(obs);
};

let test_focus_under_data_binder = () =>
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "an Int-typed variable is standard",
    [],
    focused("(fun (x: Int) -> x + 0) == (fun (x: Int) -> x)", "x + 0"),
  );

let test_focus_under_function_binder = () =>
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "a function-typed variable isn't",
    ["!! g(1)"],
    focused(
      "(fun (g: Int -> Int) -> g(1) + 0) == (fun (g: Int -> Int) -> g(1))",
      "g(1) + 0",
    ),
  );

let test_focus_under_untyped_binder = () =>
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "nor is an untyped one",
    ["!! x"],
    focused("(fun x -> x + 0) == (fun x -> x)", "x + 0"),
  );

let test_focus_under_forall = () =>
  Alcotest.check(
    Alcotest.list(Alcotest.string),
    "a quantified function is standard",
    [],
    focused("forall (g: Int -> Int) -> g(1) + 0 == g(1)", "g(1) + 0"),
  );

/* --- Applying a quantified `!!` fact --------------------------------- */

/* `f` loops, so the routine can't judge `f(t)`, though it types it a
 * list of Ints; the hypothesis `h` says it is standard for every list. Citing `refl_lemma` at `term` needs
 * `!!term`. */
let with_fact = (term: string) =>
  refl_lemma
  ++ "let f = fun (xs : [Int]) -> if f(xs) == [] then xs else xs in "
  ++ "theorem t = (forall (xs : [Int]) -> !!f(xs)) ==> forall (ys : [Int]) -> forall (bs : [Bool]) -> "
  ++ term
  ++ " == "
  ++ term
  ++ " proof assume h = forall (xs : [Int]) -> !!f(xs) => forall ys => forall bs => from "
  ++ term
  ++ " == "
  ++ term
  ++ " do rewrite same end end in t";

/* The obligations of the step citing `refl_lemma`: assuming `h` needs it
 * standard too, which the routine can't show for a looping `f`. */
let fact_conditions = (term: string): list(string) => {
  let (pm, proof) = run_named("t", with_fact(term));
  check_mark_free("nothing is refused", pm, proof);
  let rec cite = (p: Proof.t) =>
    switch (p.term) {
    | AxiomStep(_) => Some(p)
    | _ => List.find_map(~f=cite, Proof.children(p))
    };
  let obs =
    switch (
      Option.bind(cite(proof), ~f=p => ProofMap.lookup(Proof.rep_id(p), pm))
    ) {
    | Some({obligations, _}) => obligations
    | None => Alcotest.fail("no citing step")
    };
  List.map(
    ~f=
      (ob: Obligation.t) =>
        print_exp(ob.goal)
        ++ (
          switch (Obligation.remote_fact(ob)) {
          | Some((name, _)) => " by " ++ name
          | None => " " ++ Obligation.show_discharge(ob.discharge)
          }
        ),
    obs,
  );
};

let test_fact_applied = () =>
  check(
    list(string),
    "discharged by the fact",
    ["!! f(ys) by h"],
    fact_conditions("f(ys)"),
  );

let test_fact_instance_needs_standard_terms = () =>
  check(
    list(string),
    "the instance's term has to be standard",
    ["!! f([1 / 0]) by h", "0 != 0 Pending"],
    fact_conditions("f([1 / 0])"),
  );

let test_fact_applied_once = () =>
  check(
    list(string),
    "the instance's term isn't discharged by a second application",
    ["!! f(f(ys)) by h", "!! f(ys) Pending"],
    fact_conditions("f(f(ys))"),
  );

let test_fact_at_its_binder_type = () =>
  check(
    list(string),
    "a list of Bools isn't a list of Ints",
    ["!! f(bs) Pending"],
    fact_conditions("f(bs)"),
  );

/* --- Assuming an antecedent ------------------------------------------ */

/* `assume` on `P ==> Q` needs `!!P`. The step is accepted either way, and
 * the condition is left for later. */
let assumed = (src: string): list(string) => {
  let (pm, proof, obs) = obligations_named("t", src);
  check_mark_free("assume is accepted", pm, proof);
  List.map(~f=show_obligation, obs);
};

let test_assume_typed = () =>
  check(
    list(string),
    "nothing",
    [],
    assumed(
      "theorem t = forall (x : Int) -> x > 0 ==> x > 0 proof assume x > 0 => rewrite assume end in t",
    ),
  );

let test_assume_untyped = () =>
  check(
    list(string),
    "`x` could be a function",
    ["!! (x > 0) [Pending]"],
    assumed(
      "theorem t = forall x -> x > 0 ==> x > 0 proof assume x > 0 => rewrite assume end in t",
    ),
  );

let test_assume_nonstandard = () =>
  check(
    list(string),
    "a looping antecedent",
    ["!! l(0) [Pending]"],
    assumed(
      "let l = fun (u : Int) -> l(u) in theorem t = l(0) ==> l(0) proof assume l(0) => rewrite assume end in t",
    ),
  );

let test_assume_condition = () =>
  check(
    list(string),
    "a division's condition",
    ["y != 0 [Pending]"],
    assumed(
      "theorem t = forall (y : Int) -> 10 / y > 1 ==> 10 / y > 1 proof assume 10 / y > 1 => rewrite assume end in t",
    ),
  );

let tests = (
  "Evaluator.StandardInstantiation",
  [
    test_case(
      "a quantified function is applicable",
      `Quick,
      test_abstract_function_applied,
    ),
    test_case(
      "an untyped quantified function applied is left to prove",
      `Quick,
      test_abstract_function_unannotated,
    ),
    test_case(
      "fold_left with a quantified function is standard",
      `Quick,
      test_fold_left_abstract_function,
    ),
    test_case(
      "a function that divides by its argument is left to prove",
      `Quick,
      test_inv_unknown,
    ),
    test_case(
      "non-structural recursion is left to prove",
      `Quick,
      test_nonstructural_recursion_unknown,
    ),
    test_case("omega is left to prove", `Quick, test_omega_unknown),
    test_case(
      "a lambda with a divergent body is left to prove",
      `Quick,
      test_lambda_with_divergent_body_unknown,
    ),
    test_case("refl holds at omega", `Quick, test_refl_at_omega),
    test_case(
      "the quantified-function lemma is Proven",
      `Quick,
      test_function_lemma_is_proven,
    ),
    test_case(
      "instantiating at omega is left to prove",
      `Quick,
      test_instantiation_at_omega_unknown,
    ),
    test_case(
      "focus: a variable of a data type is standard",
      `Quick,
      test_focus_under_data_binder,
    ),
    test_case(
      "focus: a function-typed variable isn't standard",
      `Quick,
      test_focus_under_function_binder,
    ),
    test_case(
      "focus: an untyped variable isn't standard",
      `Quick,
      test_focus_under_untyped_binder,
    ),
    test_case(
      "focus: a forall's variable is standard",
      `Quick,
      test_focus_under_forall,
    ),
    test_case(
      "a quantified !! fact discharges an instance",
      `Quick,
      test_fact_applied,
    ),
    test_case(
      "the instance's terms have to be standard",
      `Quick,
      test_fact_instance_needs_standard_terms,
    ),
    test_case("a fact is applied once", `Quick, test_fact_applied_once),
    test_case(
      "a fact applies only at its binder's type",
      `Quick,
      test_fact_at_its_binder_type,
    ),
    test_case(
      "assuming a typed antecedent needs nothing",
      `Quick,
      test_assume_typed,
    ),
    test_case(
      "assuming an untyped antecedent leaves !!P",
      `Quick,
      test_assume_untyped,
    ),
    test_case(
      "assuming a looping antecedent leaves !!P",
      `Quick,
      test_assume_nonstandard,
    ),
    test_case(
      "assuming an antecedent incurs its conditions",
      `Quick,
      test_assume_condition,
    ),
  ],
);
