open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/**
 * Integration tests for probe sample selection.
 *
 * These evaluate real programs, get real samples from the evaluator,
 * then simulate cursor/pin states and verify that Selection.select
 * returns the expected results. This tests the full pipeline:
 *   parse -> evaluate -> get samples -> filter_by_pin -> select
 *
 * Complements Test_SampleSelection.re (unit tests with hand-crafted data)
 * by using real evaluation output, catching mismatches between what the
 * evaluator produces and what the selection logic expects.
 */

/* --- Helpers --- */

/* Get probes map (keyed by probe id) from evaluated code */
let get_probes_map = (code: string): Id.Map.t(list(Sample.t)) => {
  let (term, info_map, targets) = parse_with_probes(code);
  let elaborated = elaborate_with_info(info_map, term);
  let (_, state) =
    Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
  EvaluatorState.get_probes(state);
};

/* Get all samples flat from evaluated code */
let get_all_samples = (code: string): list(Sample.t) =>
  get_probes_map(code) |> Id.Map.bindings |> List.concat_map(snd);

/* Partition samples into top-level (empty stack) and inner (non-empty stack) */
let partition_by_depth =
    (samples: list(Sample.t)): (list(Sample.t), list(Sample.t)) =>
  List.partition(
    (s: Sample.t) => List.length(s.call_stack) == 0,
    samples,
  );

/* Make a cursor at a given stack, with optional pin */
let mk_cursor =
    (~pinned=None, ~indicated_call=None, stack: Sample.call_stack)
    : Sample.Cursor.t => {
  call_stack: stack,
  index: List.length(stack) - 1,
  pinned_stack: pinned,
  indicated_call,
  time: None,
  seq: 0,
  step_range: None,
  pending_focus: None,
};

/* Run Selection.select and return (count, total_before_filter) */
let run_select =
    (
      ~mode=Sample.Window.Single,
      ~ap_id=None,
      ~cursor: Sample.Cursor.t,
      samples: list(Sample.t),
    )
    : (list(Sample.t), int) => {
  let (selected, _offset) =
    Sample.Selection.select(
      ~mode,
      ~offset=0,
      ~ap_id,
      ~pinned=cursor.pinned_stack,
      ~cursor,
      samples,
    );
  (selected, List.length(samples));
};

/* --- Tests: top-level cursor sees top-level samples --- */

let top_level_tests = [
  test_case(
    "Top-level cursor selects top-level sample",
    `Quick,
    () => {
      let samples =
        get_all_samples({|let f = fun x -> x + 1
in ^^probe(f(5))|});
      let cursor = mk_cursor([]);
      let (selected, _) = run_select(~cursor, samples);
      check(
        int,
        "should select 1 sample in single mode",
        1,
        List.length(selected),
      );
    },
  ),
  test_case(
    "Top-level cursor sees all samples in Many mode",
    `Quick,
    () => {
      let samples =
        get_all_samples(
          {|let f = fun x -> x
in ^^probe(f(1)); ^^probe(f(2)); ^^probe(f(3))|},
        );
      let cursor = mk_cursor([]);
      let (selected, _) = run_select(~mode=Many, ~cursor, samples);
      check(
        int,
        "should see all 3 samples in many mode",
        3,
        List.length(selected),
      );
    },
  ),
];

/* --- Tests: step-into simulation --- */

let step_into_tests = [
  test_case(
    "Step-into: inner samples visible after pin (regression)",
    `Quick,
    () => {
      /* Simulate: user is at ^^probe(f(5)), steps into f.
       * Inside f, there's ^^probe(x).
       * After step-into, cursor and pin are set to [(ap_id, None)].
       * The evaluator's samples for the inner probe have [(ap_id, Some("f"))].
       * Selection should find the matching sample despite None vs Some name. */
      let code = {|let f = fun x -> ^^probe(x)
in f(5)|};
      let samples = get_all_samples(code);
      /* All samples should be inside the function (non-empty call stack) */
      switch (samples) {
      | [s] =>
        check(
          bool,
          "sample should have non-empty call stack",
          true,
          List.length(s.call_stack) > 0,
        );
        /* Simulate step-into: cursor has same stack but with None name */
        let cursor_stack =
          List.map(
            (f: Sample.stack_frame): Sample.stack_frame =>
              {id: f.id, name: None},
            s.call_stack,
          );
        let cursor =
          mk_cursor(~pinned=Some(cursor_stack), cursor_stack);
        let (selected, _) = run_select(~cursor, samples);
        check(
          int,
          "should select the inner sample (was 0 before regression fix)",
          1,
          List.length(selected),
        );
      | _ =>
        fail(
          "Expected 1 sample, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
  test_case(
    "Step-into: pin filters to correct call context",
    `Quick,
    () => {
      /* Two calls to f: f(1) and f(2), probe inside f.
       * After stepping into f(1), pin should show only f(1)'s sample. */
      let code = {|let f = fun x -> ^^probe(x)
in f(1); f(2)|};
      let samples = get_all_samples(code);
      check(
        int,
        "should have 2 samples (one per call)",
        2,
        List.length(samples),
      );
      /* Pick first sample's stack, simulate step-into with None names */
      let first = List.hd(samples);
      let pin_stack =
        List.map(
          (f: Sample.stack_frame): Sample.stack_frame =>
            {id: f.id, name: None},
          first.call_stack,
        );
      let cursor = mk_cursor(~pinned=Some(pin_stack), pin_stack);
      let (selected, _) = run_select(~cursor, samples);
      check(
        int,
        "pin should filter to 1 sample",
        1,
        List.length(selected),
      );
    },
  ),
  test_case(
    "Step-into: nested calls visible at correct depth",
    `Quick,
    () => {
      /* f calls g: step into f(5), then see g's samples.
       * Samples inside g should have 2-deep call stacks.
       * Cursor at depth 1 (just f) should see g's samples as Below. */
      let code = {|let g = fun y -> ^^probe(y)
in let f = fun x -> g(x + 1)
in f(5)|};
      let samples = get_all_samples(code);
      switch (samples) {
      | [s] =>
        check(
          bool,
          "inner sample should have depth >= 1",
          true,
          List.length(s.call_stack) >= 1,
        );
        /* Cursor at the shallowest frame only */
        let outermost_frame =
          List.rev(s.call_stack) |> List.hd;
        let shallow_stack = [{...outermost_frame, name: None}];
        let cursor =
          mk_cursor(~pinned=Some(shallow_stack), shallow_stack);
        /* In Many mode, the sample should be visible (it's Below cursor) */
        let (selected, _) = run_select(~mode=Many, ~cursor, samples);
        check(
          int,
          "deeper sample visible in many mode",
          1,
          List.length(selected),
        );
      | _ =>
        fail(
          "Expected 1 sample, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* --- Tests: pin interaction with multiple probes --- */

let pin_integration_tests = [
  test_case(
    "Pin at call A hides samples from call B",
    `Quick,
    () => {
      /* Two calls to f, probe inside f.
       * Pin at call A's stack should hide call B's samples. */
      let code = {|let f = fun x -> ^^probe(x)
in f(1); f(2)|};
      let samples = get_all_samples(code);
      check(int, "should have 2 samples", 2, List.length(samples));
      let (s1, s2) = (List.nth(samples, 0), List.nth(samples, 1));
      /* Pin to s1's context, with None names (as step-into would) */
      let pin_stack =
        List.map(
          (f: Sample.stack_frame): Sample.stack_frame =>
            {id: f.id, name: None},
          s1.call_stack,
        );
      let cursor = mk_cursor(~pinned=Some(pin_stack), pin_stack);
      /* filter_by_pin should keep s1 and drop s2 */
      let filtered =
        Sample.Selection.filter_by_pin(
          ~ap_id=None,
          ~pinned=Some(pin_stack),
          samples,
        );
      check(
        int,
        "pin should filter to 1 sample",
        1,
        List.length(filtered),
      );
      /* Verify it's s1, not s2 (by matching call stack) */
      let kept = List.hd(filtered);
      check(
        bool,
        "kept sample should match s1's call stack",
        true,
        Sample.equal_call_stack(kept.call_stack, s1.call_stack),
      );
      check(
        bool,
        "kept sample should NOT match s2's call stack",
        false,
        Sample.equal_call_stack(kept.call_stack, s2.call_stack),
      );
      /* Full select should also return 1 */
      let (selected, _) = run_select(~cursor, samples);
      check(int, "select returns 1", 1, List.length(selected));
    },
  ),
  test_case(
    "No pin shows all samples from all calls",
    `Quick,
    () => {
      let code = {|let f = fun x -> ^^probe(x)
in f(1); f(2); f(3)|};
      let samples = get_all_samples(code);
      check(int, "should have 3 samples", 3, List.length(samples));
      let filtered =
        Sample.Selection.filter_by_pin(~ap_id=None, ~pinned=None, samples);
      check(
        int,
        "no pin keeps all samples",
        3,
        List.length(filtered),
      );
    },
  ),
];

/* --- Tests: cursor relation with real samples --- */

let relation_integration_tests = [
  test_case(
    "Cursor.relation correctly classifies real samples",
    `Quick,
    () => {
      /* Probe at top level and inside function.
       * Top-level cursor: inner sample is Below, outer sample is Same. */
      let code = {|let f = fun x -> ^^probe(x)
in ^^probe(f(5))|};
      let samples = get_all_samples(code);
      let (top_samples, inner_samples) = partition_by_depth(samples);
      check(
        bool,
        "should have top-level and inner samples",
        true,
        List.length(top_samples) > 0 && List.length(inner_samples) > 0,
      );
      /* Top-level cursor should see top samples as Same/related */
      let cursor = mk_cursor([]);
      let top_sample = List.hd(top_samples);
      let inner_sample = List.hd(inner_samples);
      let top_rel =
        Sample.Cursor.relation(
          ~trimmed=false,
          ~ap_id=None,
          cursor,
          top_sample,
        );
      let inner_rel =
        Sample.Cursor.relation(
          ~trimmed=false,
          ~ap_id=None,
          cursor,
          inner_sample,
        );
      check(
        bool,
        "top sample is Same level as top cursor",
        true,
        top_rel.relative_level_to_cursor == Same,
      );
      check(
        bool,
        "inner sample is Below top cursor",
        true,
        switch (inner_rel.relative_level_to_cursor) {
        | Below(_) => true
        | _ => false
        },
      );
    },
  ),
  test_case(
    "Cursor at inner depth sees outer sample as Above",
    `Quick,
    () => {
      let code = {|let f = fun x -> ^^probe(x)
in f(5)|};
      let samples = get_all_samples(code);
      switch (samples) {
      | [s] =>
        /* Cursor at the inner call stack depth */
        let cursor = mk_cursor(s.call_stack);
        /* A hypothetical top-level sample */
        let top_sample: Sample.t = {...s, call_stack: []};
        let rel =
          Sample.Cursor.relation(
            ~trimmed=false,
            ~ap_id=None,
            cursor,
            top_sample,
          );
        check(
          bool,
          "top sample is Above inner cursor",
          true,
          switch (rel.relative_level_to_cursor) {
          | Above(_) => true
          | _ => false
          },
        );
      | _ =>
        fail(
          "Expected 1 sample, got " ++ string_of_int(List.length(samples)),
        )
      };
    },
  ),
];

/* --- Tests: Single vs Many mode with real data --- */

let mode_tests = [
  test_case(
    "Single mode: shows exactly 1 from multiple top-level samples",
    `Quick,
    () => {
      let code =
        {|let f = fun x -> x * 2
in ^^probe(f(1)); ^^probe(f(2)); ^^probe(f(3))|};
      let samples = get_all_samples(code);
      check(
        bool,
        "should have multiple samples",
        true,
        List.length(samples) >= 3,
      );
      let cursor = mk_cursor([]);
      let (selected, _) = run_select(~mode=Single, ~cursor, samples);
      check(int, "single mode shows 1", 1, List.length(selected));
    },
  ),
  test_case(
    "Many mode: shows all from multiple top-level samples",
    `Quick,
    () => {
      let code =
        {|let f = fun x -> x * 2
in ^^probe(f(1)); ^^probe(f(2)); ^^probe(f(3))|};
      let samples = get_all_samples(code);
      let n = List.length(samples);
      let cursor = mk_cursor([]);
      let (selected, _) = run_select(~mode=Many, ~cursor, samples);
      check(int, "many mode shows all", n, List.length(selected));
    },
  ),
];

let tests = (
  "Evaluator.ProbeSelection",
  List.concat([
    top_level_tests,
    step_into_tests,
    pin_integration_tests,
    relation_integration_tests,
    mode_tests,
  ]),
);
