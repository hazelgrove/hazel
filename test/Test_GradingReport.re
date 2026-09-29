open Alcotest;
open Haz3lcore;
open Poly;

let approx_eq = (a, b) => Float.abs(a -. b) < 0.01;

let score =
  testable(
    (ppf, (earned, max)) => Fmt.pf(ppf, "(%.1f, %.1f)", earned, max),
    ((e1, m1), (e2, m2)) => approx_eq(e1, e2) && approx_eq(m1, m2),
  );

let max_points_code = () => {
  let spec: Web.CodeExercise.spec =
    Web.CodeExercise.blank_spec(
      ~title="test",
      ~module_name="test",
      ~point_distribution={
        test_validation: 10,
        mutation_testing: 40,
        impl_grading: 50,
      },
      ~required_tests=0,
      ~provided_tests=0,
      ~num_wrong_impls=0,
    );
  check(
    int,
    "code max_points",
    100,
    Web.Exercise.max_points_of(Code(spec)),
  );
};

let max_points_derivation = () => {
  let spec =
    Web.DerivationExercise.blank_spec(~title="test", ~module_name="test");
  check(
    int,
    "derivation max_points",
    spec.max_points,
    Web.Exercise.max_points_of(Derivation(spec)),
  );
};

let max_points_theorem = () => {
  let spec =
    Web.TheoremExercise.blank_spec(~title="test", ~module_name="test");
  check(
    int,
    "theorem max_points",
    spec.max_points,
    Web.Exercise.max_points_of(Theorem(spec)),
  );
};

let score_of_percent_full = () => {
  check(
    score,
    "100% of 50 points",
    (50.0, 50.0),
    Web.Grading.score_of_percent(1.0, 50),
  );
};

let score_of_percent_half = () => {
  check(
    score,
    "50% of 40 points",
    (20.0, 40.0),
    Web.Grading.score_of_percent(0.5, 40),
  );
};

let score_of_percent_zero = () => {
  check(
    score,
    "0% of 10 points",
    (0.0, 10.0),
    Web.Grading.score_of_percent(0.0, 10),
  );
};

let drv_blank_grading = () => {
  let spec =
    Web.DerivationExercise.blank_spec(~title="test", ~module_name="test");
  let persistent: Web.DerivationExercise.persistent_state =
    Web.DerivationExercise.map(spec, z => PersistentZipper.persist(z));
  let report = Web.GradeExercise.grade_derivation(spec, persistent);
  let (earned, max) = report.overall;
  check(
    float(0.01),
    "blank derivation max points matches spec",
    float_of_int(spec.max_points),
    max,
  );
  check(
    float(0.01),
    "blank derivation earns 0 (all-or-nothing)",
    0.0,
    earned,
  );
};

let drv_all_or_nothing = () => {
  let spec =
    Web.DerivationExercise.blank_spec(~title="test", ~module_name="test");
  let max = float_of_int(spec.max_points);
  check(
    score,
    "empty verified tree scores 0",
    (0.0, max),
    Web.GradeExercise.score_of_verified_tree(spec, []),
  );
};

/* Each of the documentation-mode derivation slides is a fully completed
   derivation. Grading each as if it were an exercise submission should
   produce full credit (earned == max). If grading ever regresses or a
   slide's content drifts from being a valid proof, these tests will fail. */
let check_drv_slide_full_credit =
    (~name: string, spec: Web.DerivationExercise.spec, ()) => {
  let persistent: Web.DerivationExercise.persistent_state =
    Web.DerivationExercise.map(spec, z => PersistentZipper.persist(z));
  let report = Web.GradeExercise.grade_derivation(spec, persistent);
  let max = float_of_int(spec.max_points);
  check(score, name ++ " gets full credit", (max, max), report.overall);
};

let documentation_drv_slide_full_credit_cases =
  [
    (
      "Curried Function Derivation",
      Web.Ex_Curried_Function_Derivation.exercise,
    ),
    ("PairMap Derivation", Web.Ex_PairMap_Derivation.exercise),
    ("Shadowing and Closures", Web.Ex_Shadowing_And_Closures.exercise),
    (
      "Type Validation Derivation",
      Web.Ex_Type_Validation_Derivation.exercise,
    ),
    ("Conjunction Commutativity", Web.Ex_Conjunction_Commutativity.exercise),
  ]
  |> List.map(~f=((name, spec)) =>
       test_case(
         "doc slide full credit: " ++ name,
         `Quick,
         check_drv_slide_full_credit(~name, spec),
       )
     );

let thm_blank_grading = () => {
  let spec =
    Web.TheoremExercise.blank_spec(~title="test", ~module_name="test");
  let report = Web.GradeExercise.grade_theorem(spec, ());
  let (earned, max) = report.overall;
  check(
    float(0.01),
    "theorem max matches spec",
    float_of_int(spec.max_points),
    max,
  );
  check(float(0.01), "theorem earned is 0 (manual grading)", 0.0, earned);
};

let tests = (
  "GradingReport",
  [
    test_case("max_points_of Code", `Quick, max_points_code),
    test_case("max_points_of Derivation", `Quick, max_points_derivation),
    test_case("max_points_of Theorem", `Quick, max_points_theorem),
    test_case("score_of_percent full", `Quick, score_of_percent_full),
    test_case("score_of_percent half", `Quick, score_of_percent_half),
    test_case("score_of_percent zero", `Quick, score_of_percent_zero),
    test_case("blank derivation grading", `Quick, drv_blank_grading),
    test_case("derivation all-or-nothing", `Quick, drv_all_or_nothing),
    test_case("blank theorem grading", `Quick, thm_blank_grading),
  ]
  @ documentation_drv_slide_full_credit_cases,
);
