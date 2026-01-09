open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/**
 * Tests for probe step range relationships.
 *
 * Probes are created using the ^^probe(...) syntax.
 *
 * These tests verify the relative temporal relationships between probe samples:
 * - DisjointBefore: sample A finished before sample B started
 * - DisjointAfter: sample A started after sample B finished
 * - ContainedWithin: sample A's range is inside sample B's range
 * - Contains: sample A's range contains sample B's range
 * - Equal: same step range
 */

/* Relationship predicates */

let disjoint_before = (a: Sample.t, b: Sample.t): bool =>
  a.step_end < b.step_start;

let disjoint_after = (a: Sample.t, b: Sample.t): bool =>
  a.step_start > b.step_end;

let contained_within = (a: Sample.t, b: Sample.t): bool =>
  b.step_start <= a.step_start && a.step_end <= b.step_end;

let contains = (a: Sample.t, b: Sample.t): bool =>
  a.step_start <= b.step_start && b.step_end <= a.step_end;

type relationship =
  | DisjointBefore
  | DisjointAfter
  | ContainedWithin
  | Contains
  | Equal;

let classify = (a: Sample.t, b: Sample.t): relationship =>
  if (disjoint_before(a, b)) {
    DisjointBefore;
  } else if (disjoint_after(a, b)) {
    DisjointAfter;
  } else if (a.step_start == b.step_start && a.step_end == b.step_end) {
    Equal;
  } else if (contained_within(a, b)) {
    ContainedWithin;
  } else {
    Contains;
  };

let show_relationship =
  fun
  | DisjointBefore => "DisjointBefore"
  | DisjointAfter => "DisjointAfter"
  | ContainedWithin => "ContainedWithin"
  | Contains => "Contains"
  | Equal => "Equal";

let relationship_testable =
  testable(Fmt.using(show_relationship, Fmt.string), (==));

/* Helper to get all samples from evaluated code with probes */
let get_all_samples = (code: string): list(Sample.t) => {
  let (term, info_map, probe_map) = parse_with_probes(code);
  let elaborated = elaborate_with_info(info_map, term);
  let (_, state) =
    Evaluator.evaluate(~probe_map, ~env=Builtins.env_init, elaborated);
  let probes = EvaluatorState.get_probes(state);
  Id.Map.bindings(probes) |> List.concat_map(snd);
};

/* Basic step range tests */
let basic_tests = [
  test_case(
    "Probe sample has valid step range",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1 + 2)|});
      switch (samples) {
      | [s] =>
        check(
          bool,
          "step_end >= step_start",
          true,
          s.step_end >= s.step_start,
        );
        check(bool, "step_start >= 0", true, s.step_start >= 0);
      | _ => fail("Expected exactly one sample")
      };
    },
  ),
  test_case(
    "Multi-step expression has wider range",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1 + 2 + 3)|});
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Range spans multiple steps",
          true,
          s.step_end > s.step_start,
        )
      | _ => fail("Expected exactly one sample")
      };
    },
  ),
];

/* Sequential probe tests - verify step ranges are disjoint */
let sequential_tests = [
  test_case(
    "Sequential probes have disjoint step ranges",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1) + ^^probe(2) + ^^probe(3)|});
      /* All pairs should be disjoint */
      let rec check_disjoint =
        fun
        | []
        | [_] => ()
        | [a, b, ...rest] => {
            check(
              bool,
              "Pairwise disjoint",
              true,
              disjoint_before(a, b) || disjoint_after(a, b),
            );
            check_disjoint([b, ...rest]);
          };
      check_disjoint(samples);
    },
  ),
];

/* Nesting tests - verify step range containment */
let nesting_tests = [
  test_case(
    "Inner probe step range contained within outer",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1 + ^^probe(2 + 3))|});
      /* Sort by step_start - outer should start earlier */
      let sorted =
        List.sort(
          (a: Sample.t, b: Sample.t) => compare(a.step_start, b.step_start),
          samples,
        );
      switch (sorted) {
      | [outer, inner] =>
        check(
          bool,
          "Inner contained within outer",
          true,
          contained_within(inner, outer),
        )
      | _ => fail("Expected exactly two samples")
      };
    },
  ),
];

/* Recursive function tests */
let recursive_tests = [
  test_case(
    "Outer recursive calls contain inner calls",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|
        let sum = fun n =>
          if n <= 0 then 0 else n + ^^probe(sum(n - 1))
        in sum(3)
      |},
        );

      /* Sort by step_start (earlier = outer call) */
      let sorted =
        List.sort(
          (a: Sample.t, b: Sample.t) => compare(a.step_start, b.step_start),
          samples,
        );

      switch (sorted) {
      | [first, ...rest] when List.length(rest) > 0 =>
        /* First (outermost) should contain all others */
        List.iter(
          (inner: Sample.t) =>
            check(
              bool,
              "Outer contains inner",
              true,
              contains(first, inner),
            ),
          rest,
        )
      | _ => () /* Not enough samples */
      };
    },
  ),
];

/* Compound expression step range tests (when fixed, these should verify multi-step ranges) */
let compound_tests = [
  test_case(
    "Probe on if should span multiple steps",
    `Quick,
    () => {
      /* If expression should span from condition eval to final value */
      let samples = get_all_samples({|^^probe(if 1 == 1 then 100 else 200)|});
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Range spans condition and branch",
          true,
          s.step_end > s.step_start,
        )
      | _ => fail("Expected exactly one sample")
      };
    },
  ),
  test_case(
    "Probe on let should span multiple steps",
    `Quick,
    () => {
      /* Let expression should span from binding eval to body result */
      let samples = get_all_samples({|^^probe(let x = 1 + 2 in x * 3)|});
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Range spans binding and body",
          true,
          s.step_end > s.step_start,
        )
      | _ => fail("Expected exactly one sample")
      };
    },
  ),
];

let tests = (
  "Evaluator.ProbeSteps",
  List.concat([
    basic_tests,
    sequential_tests,
    nesting_tests,
    recursive_tests,
    compound_tests,
  ]),
);
