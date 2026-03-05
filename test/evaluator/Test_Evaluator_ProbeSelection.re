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
  List.partition((s: Sample.t) => List.length(s.call_stack) == 0, samples);

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
              {
                id: f.id,
                name: None,
                fn_def_id: None,
              },
            s.call_stack,
          );
        let cursor = mk_cursor(~pinned=Some(cursor_stack), cursor_stack);
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
            {
              id: f.id,
              name: None,
              fn_def_id: None,
            },
          first.call_stack,
        );
      let cursor = mk_cursor(~pinned=Some(pin_stack), pin_stack);
      let (selected, _) = run_select(~cursor, samples);
      check(int, "pin should filter to 1 sample", 1, List.length(selected));
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
        let outermost_frame = List.rev(s.call_stack) |> List.hd;
        let shallow_stack = [
          {
            ...outermost_frame,
            name: None,
            fn_def_id: None,
          },
        ];
        let cursor = mk_cursor(~pinned=Some(shallow_stack), shallow_stack);
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
            {
              id: f.id,
              name: None,
              fn_def_id: None,
            },
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
      check(int, "pin should filter to 1 sample", 1, List.length(filtered));
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
      check(int, "no pin keeps all samples", 3, List.length(filtered));
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
        let top_sample: Sample.t = {
          ...s,
          call_stack: [],
        };
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
      let code = {|let f = fun x -> x * 2
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
      let code = {|let f = fun x -> x * 2
in ^^probe(f(1)); ^^probe(f(2)); ^^probe(f(3))|};
      let samples = get_all_samples(code);
      let n = List.length(samples);
      let cursor = mk_cursor([]);
      let (selected, _) = run_select(~mode=Many, ~cursor, samples);
      check(int, "many mode shows all", n, List.length(selected));
    },
  ),
];

/* --- Tests: intent preservation with nested function calls --- */

/* Helper: mk_cursor with explicit index for intent preservation testing */
let mk_cursor_at_index =
    (
      ~pinned=None,
      ~indicated_call=None,
      ~index: int,
      stack: Sample.call_stack,
    )
    : Sample.Cursor.t => {
  call_stack: stack,
  index,
  pinned_stack: pinned,
  indicated_call,
  time: None,
  seq: 0,
  step_range: None,
  pending_focus: None,
};

let intent_preservation_tests = [
  test_case(
    "Intent preservation: inner selection preserved when clicking outer probe",
    `Quick,
    () => {
      /* Program: function called 3 times with probe inside.
       * When user selects inner sample 1, then clicks outer probe
       * (lowering cursor index), inner probe should still show sample 1. */
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in [f(1), f(2), f(3)]|};
      let inner_samples = get_all_samples(code);
      check(
        int,
        "should have 3 samples (one per call)",
        3,
        List.length(inner_samples),
      );
      /* All samples should have non-empty call stacks (inside f) */
      check(
        bool,
        "all samples should have depth >= 1",
        true,
        List.for_all(
          (s: Sample.t) => List.length(s.call_stack) >= 1,
          inner_samples,
        ),
      );
      /* Simulate: user selected inner sample 1, then clicked outer probe.
       * Cursor has full stack from sample 1 but index lowered to outer level. */
      let sample_1 = List.nth(inner_samples, 1);
      let outer_index = max(0, List.length(sample_1.call_stack) - 2);
      let cursor =
        mk_cursor_at_index(~index=outer_index, sample_1.call_stack);
      /* most_aligned_index should find sample 1, not 0 */
      let found_idx =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "should preserve inner selection at index 1 (not reset to 0)",
        true,
        found_idx == Some(1),
      );
    },
  ),
  test_case(
    "Intent preservation: arrow navigation from preserved position",
    `Quick,
    () => {
      /* Same setup: verify that arrow navigation starts from the
       * preserved position, not from 0. */
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in [f(10), f(20), f(30)]|};
      let inner_samples = get_all_samples(code);
      check(int, "should have 3 samples", 3, List.length(inner_samples));
      /* Select sample 2, lower index to outer level */
      let sample_2 = List.nth(inner_samples, 2);
      let outer_index = max(0, List.length(sample_2.call_stack) - 2);
      let cursor =
        mk_cursor_at_index(~index=outer_index, sample_2.call_stack);
      let cursor_idx =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "cursor should be at preserved position (index 2)",
        true,
        cursor_idx == Some(2),
      );
    },
  ),
];

/* --- Tests: call-click alignment with real evaluation --- */
/* Scenario: f called 3 times, probe inside f, probes on each call.
 * Clicking on a call probe should align the inner probe to that call's sample.
 * This tests whether indicated_call (set from ap_id) correctly discriminates. */

let call_click_alignment_tests = [
  test_case(
    "Call-click: indicated_call from real app IDs aligns inner probe",
    `Quick,
    () => {
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in ^^probe(f(1))
+ ^^probe(f(2))
+ ^^probe(f(3))|};
      let all_samples = get_all_samples(code);
      let (_call_samples, inner_samples) = partition_by_depth(all_samples);
      check(
        int,
        "should have 3 inner samples (one per call)",
        3,
        List.length(inner_samples),
      );
      /* Each inner sample has a 1-frame call stack. The frame ID is the
       * application site ID. Setting indicated_call to that ID should
       * cause most_aligned_index to find exactly that sample. */
      List.iteri(
        (i, inner_sample: Sample.t) => {
          check(
            bool,
            "inner sample should have 1-frame call stack",
            true,
            List.length(inner_sample.call_stack) == 1,
          );
          let app_id = List.hd(inner_sample.call_stack).id;
          let cursor = mk_cursor(~indicated_call=Some(app_id), []);
          let result =
            Sample.Selection.most_aligned_index(
              ~ap_id=None,
              cursor,
              inner_samples,
            );
          switch (result) {
          | Some(idx) =>
            let found = List.nth(inner_samples, idx);
            check(
              bool,
              Printf.sprintf(
                "sample %d: aligned sample should match clicked call",
                i,
              ),
              true,
              List.hd(found.call_stack).id == app_id,
            );
          | None =>
            fail(
              Printf.sprintf(
                "sample %d: should find an aligned inner sample",
                i,
              ),
            )
          };
        },
        inner_samples,
      );
    },
  ),
  test_case(
    "Call-click: select Single returns correct sample with indicated_call",
    `Quick,
    () => {
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in ^^probe(f(10))
+ ^^probe(f(20))
+ ^^probe(f(30))|};
      let all_samples = get_all_samples(code);
      let (_call_samples, inner_samples) = partition_by_depth(all_samples);
      check(
        int,
        "should have 3 inner samples",
        3,
        List.length(inner_samples),
      );
      /* Pick the second inner sample, set indicated_call to its app ID */
      let target = List.nth(inner_samples, 1);
      let app_id = List.hd(target.call_stack).id;
      let cursor = mk_cursor(~indicated_call=Some(app_id), []);
      let (selected, _) = run_select(~cursor, inner_samples);
      check(
        int,
        "Single mode should show 1 sample",
        1,
        List.length(selected),
      );
      switch (selected) {
      | [s] =>
        check(
          bool,
          "selected sample should match target call",
          true,
          List.hd(s.call_stack).id == app_id,
        )
      | _ => fail("expected exactly 1 sample")
      };
    },
  ),
  test_case(
    "Call-click: without indicated_call, no call discrimination",
    `Quick,
    () => {
      /* Documents the limitation: without indicated_call, clicking
       * different call probes all show the same inner sample (first one). */
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in ^^probe(f(1))
+ ^^probe(f(2))
+ ^^probe(f(3))|};
      let all_samples = get_all_samples(code);
      let (_call_samples, inner_samples) = partition_by_depth(all_samples);
      check(
        int,
        "should have 3 inner samples",
        3,
        List.length(inner_samples),
      );
      /* Cursor with NO indicated_call */
      let cursor = mk_cursor([]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      /* Without indicated_call, falls to is_related → always index 0 */
      check(
        bool,
        "without indicated_call, always picks first",
        true,
        result == Some(0),
      );
    },
  ),
];

/* --- Tests: cur_var_ap diagnostic ---
 * Verifies that the probe's statics info has the right structure
 * for cur_var_ap to return Some(ap_id) when the probe wraps an
 * application like f(2). This is the critical link: if cur_var_ap
 * returns None, then indicated_call won't be set on click, and
 * the inner probe won't align. */

let cur_var_ap_tests = [
  test_case(
    "cur_var_ap: returns Some for probe wrapping Ap(Var)",
    `Quick,
    () => {
      let code = {|let f : (Int -> Int) = fun x -> x + 1
in ^^probe(f(2))|};
      let (_term, info_map, targets) = parse_with_probes(code);
      /* There should be exactly one probe */
      let probe_ids = Id.Map.bindings(targets) |> List.map(fst);
      check(int, "should have 1 probe", 1, List.length(probe_ids));
      let probe_id = List.hd(probe_ids);
      switch (Statics.Map.lookup(probe_id, info_map)) {
      | Some(info) =>
        let ap_id = Sample.Cursor.cur_var_ap(info);
        check(
          bool,
          "cur_var_ap should return Some for probe on f(2)",
          true,
          Option.is_some(ap_id),
        );
      | None => fail("no statics for probe ID")
      };
    },
  ),
  test_case(
    "cur_var_ap: returns None for probe on variable",
    `Quick,
    () => {
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x) + 1
in f(2)|};
      let (_term, info_map, targets) = parse_with_probes(code);
      let probe_ids = Id.Map.bindings(targets) |> List.map(fst);
      check(int, "should have 1 probe", 1, List.length(probe_ids));
      let probe_id = List.hd(probe_ids);
      switch (Statics.Map.lookup(probe_id, info_map)) {
      | Some(info) =>
        let ap_id = Sample.Cursor.cur_var_ap(info);
        check(
          bool,
          "cur_var_ap should return None for probe on variable x",
          true,
          ap_id == None,
        );
      | None => fail("no statics for probe ID")
      };
    },
  ),
  test_case(
    "cur_var_ap: app ID matches call stack frame ID from evaluator",
    `Quick,
    () => {
      /* The critical end-to-end test: verify that the ap_id from
       * cur_var_ap (which would become indicated_call via capture)
       * matches the call stack frame ID in the inner probe's samples. */
      let code = {|let f : (Int -> Int) = fun x -> ^^probe(x)
in ^^probe(f(42))|};
      let (term, info_map, targets) = parse_with_probes(code);
      /* Get probe IDs */
      let probe_ids = Id.Map.bindings(targets) |> List.map(fst);
      check(int, "should have 2 probes", 2, List.length(probe_ids));
      /* Evaluate to get samples */
      let elaborated = elaborate_with_info(info_map, term);
      let (_, state) =
        Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
      let probes_map = EvaluatorState.get_probes(state);
      /* Find the call probe (wrapping f(42)) and inner probe (on x) */
      let call_probe_id =
        List.find(
          id => {
            switch (Statics.Map.lookup(id, info_map)) {
            | Some(info) => Option.is_some(Sample.Cursor.cur_var_ap(info))
            | None => false
            }
          },
          probe_ids,
        );
      let inner_probe_id =
        List.find(
          id => {
            switch (Statics.Map.lookup(id, info_map)) {
            | Some(info) => Sample.Cursor.cur_var_ap(info) == None
            | None => false
            }
          },
          probe_ids,
        );
      /* Get ap_id from call probe's statics */
      let ap_id =
        switch (Statics.Map.lookup(call_probe_id, info_map)) {
        | Some(info) => Sample.Cursor.cur_var_ap(info)
        | None => None
        };
      check(
        bool,
        "call probe should have ap_id",
        true,
        Option.is_some(ap_id),
      );
      let ap_id = Option.get(ap_id);
      /* Get inner probe's samples */
      let inner_samples =
        switch (Id.Map.find_opt(inner_probe_id, probes_map)) {
        | Some(samples) => samples
        | None => []
        };
      check(
        int,
        "inner probe should have 1 sample",
        1,
        List.length(inner_samples),
      );
      /* The inner sample's call stack frame ID should match ap_id */
      let inner_sample = List.hd(inner_samples);
      check(
        bool,
        "inner sample should have 1-frame call stack",
        true,
        List.length(inner_sample.call_stack) == 1,
      );
      let frame_id = List.hd(inner_sample.call_stack).id;
      check(
        bool,
        "call stack frame ID should match ap_id from cur_var_ap",
        true,
        frame_id == ap_id,
      );
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
    intent_preservation_tests,
    call_click_alignment_tests,
    cur_var_ap_tests,
  ]),
);
