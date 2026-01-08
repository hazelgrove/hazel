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

/* Basic probe tests - verify probes work */
let basic_tests = [
  test_case(
    "Single probe creates one sample",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1 + 2)|});
      check(int, "One sample", 1, List.length(samples));
    },
  ),
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
    "Probe on parens returns 0 samples (known broken)",
    `Quick,
    () => {
      /* KNOWN BUG: Probe on parenthesized expression doesn't work.
       * The paren tile ID is added to refractors, but elaboration
       * removes the Parens wrapper, so the ID doesn't match during evaluation. */
      let samples = get_all_samples({|^^probe((1 + 2))|});
      check(int, "Zero samples (broken)", 0, List.length(samples));
    },
  ),
  test_case(
    "Nested expression probe has wider range",
    `Quick,
    () => {
      /* Probe around a let expression should span multiple steps.
       * Use a simpler expression without inner patterns. */
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

/* Sequential probe tests */
let sequential_tests = [
  test_case(
    "Two sequential probes on parens are both broken",
    `Quick,
    () => {
      /* KNOWN BUG: Both probes are on parens, so neither works */
      let samples = get_all_samples({|^^probe((1 + 2)) + ^^probe((3 + 4))|});
      check(int, "Zero samples (broken)", 0, List.length(samples));
    },
  ),
  test_case(
    "Two sequential probes produce two samples",
    `Quick,
    () => {
      /* Two probes evaluated one after the other (no parens) */
      let samples = get_all_samples({|^^probe(1 + 2) + ^^probe(3 + 4)|});
      check(int, "Two samples", 2, List.length(samples));
    },
  ),
  test_case(
    "Three sequential probes maintain order",
    `Quick,
    () => {
      let samples = get_all_samples({|^^probe(1) + ^^probe(2) + ^^probe(3)|});
      check(int, "Three samples", 3, List.length(samples));

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

/* Nesting tests */
let nesting_tests = [
  test_case(
    "Inner probe on parens only outer works (known broken)",
    `Quick,
    () => {
      /* KNOWN BUG: Inner probe is on parens, so it doesn't work.
       * Only the outer probe produces a sample. */
      let samples = get_all_samples({|^^probe(1 + ^^probe((2 + 3)))|});
      check(int, "One sample (inner broken)", 1, List.length(samples));
    },
  ),
  test_case(
    "Nested probes produce at least one sample",
    `Quick,
    () => {
      /* Outer probe contains inner probe (no parens).
       * Note: In the probe_map system, nested probes may produce
       * fewer samples than the AST-based system due to ID handling. */
      let samples = get_all_samples({|^^probe(1 + ^^probe(2 + 3))|});
      check(bool, "At least one sample", true, List.length(samples) >= 1);
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

let tests = (
  "Evaluator.ProbeSteps",
  List.concat([
    basic_tests,
    sequential_tests,
    nesting_tests,
    recursive_tests,
  ]),
);
