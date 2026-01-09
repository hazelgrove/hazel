open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/**
 * Tests for probe sample call_stack behavior.
 *
 * The call_stack of a probe sample represents the active function calls
 * at the time the probed expression was evaluated. This affects how samples
 * are filtered and displayed in the UI's "single" mode.
 *
 * Key behaviors:
 * - Top-level expressions should have empty call_stacks
 * - Expressions inside a function body should have the function's app_id in call_stack
 * - A probed function application should have the same call_stack as before entering the function
 */

/* Helper to get all samples from evaluated code with probes */
let get_all_samples = (code: string): list(Sample.t) => {
  let (term, info_map, probe_map) = parse_with_probes(code);
  let elaborated = elaborate_with_info(info_map, term);
  let (_, state) =
    Evaluator.evaluate(~probe_map, ~env=Builtins.env_init, elaborated);
  let probes = EvaluatorState.get_probes(state);
  Id.Map.bindings(probes) |> List.concat_map(snd);
};

/* Show call stack for debugging */
let show_call_stack = (cs: Probe.call_stack): string =>
  "[" ++ String.concat(", ", List.map(Id.str3, cs)) ++ "]";

let call_stack_testable = testable(Fmt.using(show_call_stack, Fmt.string), (==));

/* Test that multiple top-level probed applications have the same (empty) call_stack.
 * This is the bug: if they have different call_stacks (containing their own app_ids),
 * then clicking on one hides the other in "single" mode. */
let top_level_apps_tests = [
  test_case(
    "Top-level probed applications should have same call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let f = fun x -> x + 1
in ^^probe(f(1)); ^^probe(f(2))|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          call_stack_testable,
          "Both applications should have same call_stack",
          s1.call_stack,
          s2.call_stack,
        )
      | _ =>
        fail(
          "Expected exactly 2 samples, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Top-level probed application should have empty call_stack",
    `Quick,
    () => {
      let samples = get_all_samples({|let f = fun x -> x in ^^probe(f(42))|});
      switch (samples) {
      | [s] =>
        check(
          call_stack_testable,
          "Top-level app should have empty call_stack",
          [],
          s.call_stack,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Multiple top-level apps should all have empty call_stacks",
    `Quick,
    () => {
      /* This is the temperature conversion example from the bug report */
      let samples =
        get_all_samples(
          {|let celsius = fun farenheit ->
  (farenheit -. 32.) *. 5. /. 9.
in ^^probe(celsius(72.)); ^^probe(celsius(100.))|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          call_stack_testable,
          "First app should have empty call_stack",
          [],
          s1.call_stack,
        );
        check(
          call_stack_testable,
          "Second app should have empty call_stack",
          [],
          s2.call_stack,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* Test that probes inside function bodies correctly have the app_id in call_stack */
let inside_function_tests = [
  test_case(
    "Probe inside function body has app_id in call_stack",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let f = fun x -> ^^probe(x + 1)
in f(5)|},
        );
      switch (samples) {
      | [s] =>
        check(
          bool,
          "Inner probe should have non-empty call_stack",
          true,
          List.length(s.call_stack) > 0,
        )
      | _ =>
        fail(
          "Expected exactly 1 sample, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Different calls to same function have different call_stacks",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let f = fun x -> ^^probe(x)
in f(1); f(2)|},
        );
      switch (samples) {
      | [s1, s2] =>
        check(
          bool,
          "Both probes should have non-empty call_stacks",
          true,
          List.length(s1.call_stack) > 0 && List.length(s2.call_stack) > 0,
        );
        /* They should have DIFFERENT call_stacks (different app_ids) */
        check(
          bool,
          "Different calls should have different call_stacks",
          true,
          s1.call_stack != s2.call_stack,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* Test the relationship between probed app and probe inside function body */
let app_vs_body_tests = [
  test_case(
    "Probed app and probe inside function have correct relationship",
    `Quick,
    () => {
      /* The probe on the app should have empty call_stack (top-level).
       * The probe inside the function should have the app_id in call_stack.
       * The inner probe's call_stack should be the app's call_stack + app_id. */
      let samples =
        get_all_samples(
          {|let f = fun x -> ^^probe(x)
in ^^probe(f(5))|},
        );
      switch (samples) {
      | [s1, s2] =>
        /* One sample should be from the app probe (empty call_stack),
         * one from inside the function (non-empty call_stack) */
        let (app_sample, inner_sample) =
          if (List.length(s1.call_stack) == 0) {
            (s1, s2);
          } else {
            (s2, s1);
          };
        check(
          call_stack_testable,
          "App probe should have empty call_stack",
          [],
          app_sample.call_stack,
        );
        check(
          bool,
          "Inner probe should have one app_id in call_stack",
          true,
          List.length(inner_sample.call_stack) == 1,
        );
      | _ =>
        fail(
          "Expected exactly 2 samples, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

let tests = (
  "Evaluator.ProbeCallStack",
  List.concat([top_level_apps_tests, inside_function_tests, app_vs_body_tests]),
);
