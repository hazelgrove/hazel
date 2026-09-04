open Alcotest;
open Language;

/**
 * Unit tests for Sample.Selection and Sample.Focus logic.
 *
 * These test the pure functions that determine which samples are visible
 * given a cursor position, pin state, and display mode. No evaluation needed.
 *
 * The key pipeline tested:
 *   samples -> filter_by_pin -> most_aligned_index/most_aligned_sample -> select
 *
 * Key types:
 * - stack_frame = {id: Id.t, name: option(string)}
 * - call_stack = list(stack_frame)
 * - Cursor.t = {call_stack, index, pinned_stack, ...}
 * - Selection.select returns (list(Sample.t), int)
 */

/* --- Helpers --- */

/* Make a stack frame. name defaults to None (as cursor/step-into constructs) */
let frame = (~name=None, id: Id.t): CallStack.frame => {
  id,
  name,
  fn_def_id: None,
};

/* Make a named stack frame (as evaluator produces) */
let named_frame = (id: Id.t, name: string): CallStack.frame => {
  id,
  name: Some(name),
  fn_def_id: None,
};

/* Fixed IDs for readable tests */
let id_a = Id.mk();
let id_b = Id.mk();
let id_c = Id.mk();
let id_d = Id.mk();
let id_e = Id.mk();
let id_f = Id.mk();
/* Make a minimal sample with the given call stack */
let mk_sample =
    (~seq=0, ~step_start=0, ~step_end=0, stack: CallStack.t): Sample.t => {
  id: Hashtbl.hash((stack, Id.invalid)),
  syntax_id: Id.invalid,
  value: IdTagged.FreshGrammar.Exp.empty_hole(),
  env: Sample.Env.empty,
  call_stack: stack,
  args: None,
  time: 0.0,
  seq,
  origin: Probe,
  step_start,
  step_end,
};

/* Make a cursor at a given call stack depth */
let mk_cursor =
    (
      ~pinned=None,
      ~indicated_call=None,
      ~seq=0,
      ~step_range=None,
      stack: CallStack.t,
    )
    : Sample.Focus.t => {
  call_stack: stack,
  index: List.length(stack) - 1,
  pinned_stack: pinned,
  indicated_call,
  time: None,
  seq,
  step_range,
  pending_focus: None,
};

/* Count how many samples Selection.select returns */
let select_count =
    (
      ~mode=Sample.Window.Single,
      ~ap_id=None,
      ~cursor: Sample.Focus.t,
      samples: list(Sample.t),
    )
    : int => {
  let (selected, _) =
    Sample.Selection.select(
      ~mode,
      ~offset=0,
      ~ap_id,
      ~pinned=cursor.pinned_stack,
      ~cursor,
      samples,
    );
  List.length(selected);
};

/* Get the samples returned by Selection.select */
let do_select =
    (
      ~mode=Sample.Window.Single,
      ~ap_id=None,
      ~cursor: Sample.Focus.t,
      samples: list(Sample.t),
    )
    : list(Sample.t) => {
  let (selected, _) =
    Sample.Selection.select(
      ~mode,
      ~offset=0,
      ~ap_id,
      ~pinned=cursor.pinned_stack,
      ~cursor,
      samples,
    );
  selected;
};

/* --- Test: equal_stack_frame ignores name --- */

let equality_tests = [
  test_case(
    "equal_stack_frame: same id, different names",
    `Quick,
    () => {
      let f1 = frame(id_a);
      let f2 = named_frame(id_a, "foo");
      check(bool, "should be equal", true, CallStack.equal_frame(f1, f2));
    },
  ),
  test_case(
    "equal_stack_frame: different ids, same name",
    `Quick,
    () => {
      let f1 = named_frame(id_a, "foo");
      let f2 = named_frame(id_b, "foo");
      check(
        bool,
        "should not be equal",
        false,
        CallStack.equal_frame(f1, f2),
      );
    },
  ),
  test_case(
    "equal_call_stack: None vs Some names",
    `Quick,
    () => {
      /* This is the exact scenario that caused the step-into regression:
       * cursor constructs frames with name=None, evaluator produces name=Some */
      let cursor_stack = [frame(id_a), frame(id_b)];
      let eval_stack = [named_frame(id_a, "f"), named_frame(id_b, "g")];
      check(
        bool,
        "should be equal (id-only comparison)",
        true,
        CallStack.equal(cursor_stack, eval_stack),
      );
    },
  ),
];

/* --- Test: Cursor.relation --- */

let relation_tests = [
  test_case(
    "relation: Same call stack",
    `Quick,
    () => {
      let stack = [frame(id_a)];
      let cursor = mk_cursor(stack);
      let sample = mk_sample(stack);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(bool, "is_call_cursor", true, rel.is_call_cursor);
      check(
        bool,
        "relative_level is Same",
        true,
        rel.relative_level_to_cursor == Same,
      );
    },
  ),
  test_case(
    "relation: Same ids, different names (regression test)",
    `Quick,
    () => {
      /* Cursor has None names, sample has Some names */
      let cursor = mk_cursor([frame(id_a)]);
      let sample = mk_sample([named_frame(id_a, "f")]);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(bool, "is_call_cursor should be true", true, rel.is_call_cursor);
      check(
        bool,
        "relative_level should be Same",
        true,
        rel.relative_level_to_cursor == Same,
      );
    },
  ),
  test_case(
    "relation: cursor deeper than sample (Above)",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_b), frame(id_a)]);
      let sample = mk_sample([frame(id_a)]);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(bool, "is_call_cursor", false, rel.is_call_cursor);
      check(
        bool,
        "relative_level is Above",
        true,
        switch (rel.relative_level_to_cursor) {
        | Above(_) => true
        | _ => false
        },
      );
    },
  ),
  test_case(
    "relation: cursor shallower than sample (Below)",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let sample = mk_sample([frame(id_b), frame(id_a)]);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(
        bool,
        "relative_level is Below",
        true,
        switch (rel.relative_level_to_cursor) {
        | Below(_) => true
        | _ => false
        },
      );
    },
  ),
  test_case(
    "relation: unrelated stacks",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let sample = mk_sample([frame(id_b)]);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(
        bool,
        "relative_level is Unrelated",
        true,
        rel.relative_level_to_cursor == Unrelated,
      );
    },
  ),
  test_case(
    "relation: both top-level (empty stacks)",
    `Quick,
    () => {
      let cursor = mk_cursor([]);
      let sample = mk_sample([]);
      let rel =
        Sample.Focus.relation(~trimmed=false, ~ap_id=None, cursor, sample);
      check(bool, "is_call_cursor", true, rel.is_call_cursor);
      check(
        bool,
        "relative_level is Same",
        true,
        rel.relative_level_to_cursor == Same,
      );
    },
  ),
];

/* --- Test: filter_by_pin --- */

let pin_tests = [
  test_case(
    "filter_by_pin: no pin passes everything",
    `Quick,
    () => {
      let samples = [mk_sample([]), mk_sample([frame(id_a)])];
      let filtered =
        Sample.Selection.filter_by_pin(~ap_id=None, ~pinned=None, samples);
      check(int, "all samples pass", 2, List.length(filtered));
    },
  ),
  test_case(
    "filter_by_pin: pin filters to matching suffix",
    `Quick,
    () => {
      /* Pin at [A] should keep samples whose stack has A as suffix */
      let pinned = Some([frame(id_a)]);
      let s_match = mk_sample([frame(id_a)]);
      let s_deeper = mk_sample([frame(id_b), frame(id_a)]);
      let s_no_match = mk_sample([frame(id_b)]);
      let s_empty = mk_sample([]);
      let filtered =
        Sample.Selection.filter_by_pin(
          ~ap_id=None,
          ~pinned,
          [s_match, s_deeper, s_no_match, s_empty],
        );
      check(int, "should keep matching samples", 2, List.length(filtered));
    },
  ),
  test_case(
    "filter_by_pin: pin with None names matches eval names (regression)",
    `Quick,
    () => {
      /* Pin constructed by step-into has None names,
       * but samples from eval have Some names */
      let pinned = Some([frame(id_a)]);
      let sample = mk_sample([named_frame(id_a, "f")]);
      let filtered =
        Sample.Selection.filter_by_pin(~ap_id=None, ~pinned, [sample]);
      check(int, "should keep the sample", 1, List.length(filtered));
    },
  ),
  test_case(
    "filter_by_pin: step-into pin filters inside function",
    `Quick,
    () => {
      /* After stepping into f(x), viewing the probe inside f's body:
       * pin = [frame(ap_id)]
       * ap_id for the inner probe is None (it's on a variable, not a call)
       * Inner samples have stack [named_frame(ap_id, "f")]
       * Unrelated samples from a different call should be filtered out */
      let ap_id = id_a;
      let pinned = Some([frame(ap_id)]);
      let inner_sample = mk_sample([named_frame(ap_id, "f")]);
      let unrelated = mk_sample([named_frame(id_b, "g")]);
      let filtered =
        Sample.Selection.filter_by_pin(
          ~ap_id=None,
          ~pinned,
          [inner_sample, unrelated],
        );
      check(int, "should keep inner sample only", 1, List.length(filtered));
    },
  ),
];

/* --- Test: select (full pipeline) --- */

let select_tests = [
  test_case(
    "select Single: top-level cursor sees top-level sample",
    `Quick,
    () => {
      let cursor = mk_cursor([]);
      let samples = [mk_sample([])];
      let count = select_count(~cursor, samples);
      check(int, "should show 1 sample", 1, count);
    },
  ),
  test_case(
    "select Single: cursor matches one of many",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let samples = [
        mk_sample(~seq=0, [frame(id_a)]),
        mk_sample(~seq=1, [frame(id_b)]),
      ];
      let count = select_count(~cursor, samples);
      check(int, "should show 1 sample", 1, count);
    },
  ),
  test_case(
    "select Single: no matching sample returns empty",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let samples = [mk_sample([frame(id_b)])];
      let count = select_count(~cursor, samples);
      check(int, "should show 0 samples", 0, count);
    },
  ),
  test_case(
    "select Many: shows all filtered samples",
    `Quick,
    () => {
      let cursor = mk_cursor([]);
      let samples = [
        mk_sample(~seq=0, []),
        mk_sample(~seq=1, []),
        mk_sample(~seq=2, []),
      ];
      let count = select_count(~mode=Many, ~cursor, samples);
      check(int, "should show all 3 samples", 3, count);
    },
  ),
  test_case(
    "select Single: pinned + cursor with None names (regression)",
    `Quick,
    () => {
      /* The exact regression scenario:
       * 1. Step-into sets pin and cursor to [frame(ap, None)]
       * 2. Evaluator produced samples with [named_frame(ap, "f")]
       * 3. Both filter_by_pin AND selection must work */
      let ap = id_a;
      let cursor = mk_cursor(~pinned=Some([frame(ap)]), [frame(ap)]);
      let sample = mk_sample([named_frame(ap, "f")]);
      let count = select_count(~cursor, [sample]);
      check(int, "should show 1 sample (was 0 before fix)", 1, count);
    },
  ),
  test_case(
    "select Single: step-into deep call",
    `Quick,
    () => {
      /* Step into f(x) which calls g(y):
       * cursor = [frame(g_ap), frame(f_ap)]
       * pin = same
       * sample inside g has stack [named(g_ap, "g"), named(f_ap, "f")] */
      let f_ap = id_a;
      let g_ap = id_b;
      let cursor =
        mk_cursor(
          ~pinned=Some([frame(g_ap), frame(f_ap)]),
          [frame(g_ap), frame(f_ap)],
        );
      let sample =
        mk_sample([named_frame(g_ap, "g"), named_frame(f_ap, "f")]);
      let count = select_count(~cursor, [sample]);
      check(int, "should show 1 sample", 1, count);
    },
  ),
];

/* --- Test: is_same_call --- */

let is_same_call_tests = [
  test_case(
    "is_same_call: same outermost frame",
    `Quick,
    () => {
      let s1 = mk_sample([frame(id_b), frame(id_a)]);
      let s2 = mk_sample([frame(id_c), frame(id_a)]);
      check(
        bool,
        "same outermost call",
        true,
        Sample.Selection.is_same_call(s1, s2),
      );
    },
  ),
  test_case(
    "is_same_call: different outermost frame",
    `Quick,
    () => {
      let s1 = mk_sample([frame(id_b), frame(id_a)]);
      let s2 = mk_sample([frame(id_c), frame(id_d)]);
      check(
        bool,
        "different outermost call",
        false,
        Sample.Selection.is_same_call(s1, s2),
      );
    },
  ),
  test_case(
    "is_same_call: ignores function names",
    `Quick,
    () => {
      let s1 = mk_sample([named_frame(id_a, "f")]);
      let s2 = mk_sample([named_frame(id_a, "g")]);
      check(
        bool,
        "same id different names",
        true,
        Sample.Selection.is_same_call(s1, s2),
      );
    },
  ),
];

/* --- Test: most_aligned_sample --- */

let closest_tests = [
  test_case(
    "most_aligned_sample: exact match preferred",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let exact = mk_sample(~seq=0, [frame(id_a)]);
      let related = mk_sample(~seq=1, [frame(id_b), frame(id_a)]);
      let result =
        Sample.Selection.most_aligned_sample(
          ~ap_id=None,
          ~cursor,
          [related, exact],
        );
      switch (result) {
      | Some(s) => check(int, "should pick exact match", 0, s.seq)
      | None => fail("should find a sample")
      };
    },
  ),
  test_case(
    "most_aligned_sample: None vs Some names (regression)",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let sample = mk_sample(~seq=42, [named_frame(id_a, "f")]);
      let result =
        Sample.Selection.most_aligned_sample(~ap_id=None, ~cursor, [sample]);
      switch (result) {
      | Some(s) => check(int, "should find the sample", 42, s.seq)
      | None => fail("should find a sample (was failing before fix)")
      };
    },
  ),
  test_case(
    "most_aligned_sample: true suffix preferred over unrelated",
    `Quick,
    () => {
      /* Cursor at [C, A], samples at [C, A] (true suffix) and [D].
       * [C, A] is a suffix of cursor [C, A] → picked.
       * [B, A] would NOT be a suffix (B ≠ C = different branch). */
      let cursor = mk_cursor([frame(id_c), frame(id_a)]);
      let s_suffix = mk_sample(~seq=1, [frame(id_c), frame(id_a)]);
      let s_none = mk_sample(~seq=2, [frame(id_d)]);
      let result =
        Sample.Selection.most_aligned_sample(
          ~ap_id=None,
          ~cursor,
          [s_none, s_suffix],
        );
      switch (result) {
      | Some(s) => check(int, "should pick suffix match", 1, s.seq)
      | None => fail("should find a sample")
      };
    },
  ),
];

/* --- Test: empty_status --- */

let empty_status_tests = [
  test_case(
    "get_empty_status: samples shown",
    `Quick,
    () => {
      let status =
        Sample.Selection.get_empty_status(~num_total=5, ~num_shown=2, ());
      check(bool, "should be None (samples visible)", true, status == None);
    },
  ),
  test_case(
    "get_empty_status: hidden by pin",
    `Quick,
    () => {
      let status =
        Sample.Selection.get_empty_status(~num_total=0, ~num_shown=0, ());
      check(
        bool,
        "should be HiddenByPin",
        true,
        status == Some(Sample.Selection.HiddenByPin),
      );
    },
  ),
  test_case(
    "get_empty_status: not aligned",
    `Quick,
    () => {
      let status =
        Sample.Selection.get_empty_status(~num_total=5, ~num_shown=0, ());
      check(
        bool,
        "should be NotAligned",
        true,
        status == Some(Sample.Selection.NotAligned),
      );
    },
  ),
];

/* --- Test: intent preservation via most_aligned_index --- */

/* Helper: mk_cursor with explicit index (for testing intent preservation) */
let mk_cursor_at_index =
    (
      ~pinned=None,
      ~indicated_call=None,
      ~seq=0,
      ~step_range=None,
      ~index: int,
      stack: CallStack.t,
    )
    : Sample.Focus.t => {
  call_stack: stack,
  index,
  pinned_stack: pinned,
  indicated_call,
  time: None,
  seq,
  step_range,
  pending_focus: None,
};

let intent_preservation_tests = [
  test_case(
    "most_aligned_index preserves inner selection",
    `Quick,
    () => {
      /* Scenario: nested functions, outer called once, inner called 3 times.
       * User focuses inner sample 1, then clicks outer probe.
       * Cursor: full stack = [inner_1, outer_0], index = 0 (outer level).
       * Inner probe's most_aligned_index should find
       * the sample matching the full stack (index 1), not reset to 0. */
      let outer_0 = frame(id_a);
      let inner_0 = frame(id_b);
      let inner_1 = frame(id_c);
      let inner_2 = frame(id_d);
      let inner_samples = [
        mk_sample(~seq=0, [inner_0, outer_0]),
        mk_sample(~seq=1, [inner_1, outer_0]),
        mk_sample(~seq=2, [inner_2, outer_0]),
      ];
      /* Cursor at outer level but with inner_1 info preserved */
      let cursor = mk_cursor_at_index(~index=0, [inner_1, outer_0]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "should find inner_1 sample at index 1 (not reset to 0)",
        true,
        result == Some(1),
      );
    },
  ),
  test_case(
    "most_aligned_index: no preserved info falls back correctly",
    `Quick,
    () => {
      /* When cursor is genuinely at outer level (no deeper info),
       * should fall back to first related sample. */
      let outer_0 = frame(id_a);
      let inner_0 = frame(id_b);
      let inner_1 = frame(id_c);
      let inner_samples = [
        mk_sample(~seq=0, [inner_0, outer_0]),
        mk_sample(~seq=1, [inner_1, outer_0]),
      ];
      /* Cursor at outer level with NO deeper info */
      let cursor = mk_cursor([outer_0]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      /* Should find first related (index 0) via is_related fallback */
      check(
        bool,
        "should find first related sample at index 0",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "most_aligned_index: outer probe unaffected by preserved info",
    `Quick,
    () => {
      /* When cursor has inner info but we're looking at the outer probe,
       * the outer probe should still find its correct sample. */
      let outer_0 = frame(id_a);
      let outer_1 = frame(id_b);
      let inner_1 = frame(id_c);
      let outer_samples = [
        mk_sample(~seq=0, [outer_0]),
        mk_sample(~seq=1, [outer_1]),
      ];
      /* Cursor at outer level with inner info preserved */
      let cursor = mk_cursor_at_index(~index=0, [inner_1, outer_0]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          outer_samples,
        );
      /* Full stack [inner_1, outer_0] doesn't match any outer sample,
       * so [outer_0] is the longest suffix match → finds index 0. */
      check(bool, "should find outer_0 at index 0", true, result == Some(0));
    },
  ),
  test_case(
    "select Single preserves inner selection with preserved cursor",
    `Quick,
    () => {
      /* End-to-end: Selection.select should return the preserved inner sample,
       * not the first inner sample. */
      let outer_0 = frame(id_a);
      let inner_0 = frame(id_b);
      let inner_1 = frame(id_c);
      let inner_2 = frame(id_d);
      let inner_samples = [
        mk_sample(~seq=0, [inner_0, outer_0]),
        mk_sample(~seq=1, [inner_1, outer_0]),
        mk_sample(~seq=2, [inner_2, outer_0]),
      ];
      let cursor = mk_cursor_at_index(~index=0, [inner_1, outer_0]);
      /* Selection.select uses most_aligned_index internally. */
      let selected = do_select(~cursor, inner_samples);
      check(int, "should show 1 sample", 1, List.length(selected));
      switch (selected) {
      | [s] => check(int, "should be inner_1 (seq=1)", 1, s.seq)
      | _ => fail("expected exactly 1 sample")
      };
    },
  ),
  test_case(
    "move_cursor simulation: arrow from preserved position",
    `Quick,
    () => {
      /* Simulates what ProbeProj.move_cursor does:
       * most_aligned_index to find current position,
       * then offset ±1 to get next sample. With intent preservation,
       * current position should be at the preserved sample, not reset. */
      let outer_0 = frame(id_a);
      let inner_0 = frame(id_b);
      let inner_1 = frame(id_c);
      let inner_2 = frame(id_d);
      let inner_samples = [
        mk_sample(~seq=0, [inner_0, outer_0]),
        mk_sample(~seq=1, [inner_1, outer_0]),
        mk_sample(~seq=2, [inner_2, outer_0]),
      ];
      let cursor = mk_cursor_at_index(~index=0, [inner_1, outer_0]);
      let cursor_idx =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      /* Should be at index 1 (inner_1), not index 0 */
      check(
        bool,
        "cursor should be at index 1",
        true,
        cursor_idx == Some(1),
      );
      /* Arrow right (offset=-1): next_idx = 1 + 1 = 2 (inner_2) */
      let next_idx =
        switch (cursor_idx) {
        | Some(idx) => Some(idx + 1)
        | None => None
        };
      check(
        bool,
        "next sample should be index 2",
        true,
        next_idx == Some(2),
      );
    },
  ),
];

/* Three-level intent preservation tests.
 *
 * Scenario: fun xs -> map(fun x -> map(fun y -> ..., inner), xs)
 * called once. Three levels of call stacks:
 *   Top: [F]           (inside f call)
 *   Mid: [Mk, F]       (inside kth outer-map iteration)
 *   Inner: [Nj, Mk, F] (inside jth inner-map iteration under Mk)
 *
 * The bug: navigating inner→mid→top preserves both inner (Nj) and
 * mid (Mk) info in the cursor stack. But when the mid-level probe
 * re-renders at the top level, only the two-level mechanism was used:
 * full match ([Nj,Mk,F] ≠ [Mk,F]) and trimmed match ([F] ≠ [Mk,F])
 * both fail, falling to "any related" which picks M0 instead of Mk.
 * The fix adds an intermediate depth match tier. */
let three_level_tests = [
  test_case(
    "three-level: mid probe finds correct sample from full cursor stack",
    `Quick,
    () => {
      let f_frame = frame(id_a);
      let m0 = frame(id_b);
      let m1 = frame(id_c);
      let m2 = frame(id_d);
      let n0 = frame(id_e);
      let mid_samples = [
        mk_sample(~seq=0, [m0, f_frame]),
        mk_sample(~seq=1, [m1, f_frame]),
        mk_sample(~seq=2, [m2, f_frame]),
      ];
      /* Cursor at top level with inner+mid info preserved:
       * [N0, M1, F] at index=0 (viewing top level) */
      let cursor = mk_cursor_at_index(~index=0, [n0, m1, f_frame]);
      let result =
        Sample.Selection.most_aligned_index(~ap_id=None, cursor, mid_samples);
      check(
        bool,
        "should find M1 at index 1 via intermediate match (not M0 at 0)",
        true,
        result == Some(1),
      );
    },
  ),
  test_case(
    "three-level: inner probe still found via full match",
    `Quick,
    () => {
      let f_frame = frame(id_a);
      let m1 = frame(id_c);
      let n0 = frame(id_e);
      let n1 = frame(id_f);
      let inner_samples = [
        mk_sample(~seq=0, [n0, m1, f_frame]),
        mk_sample(~seq=1, [n1, m1, f_frame]),
      ];
      /* Cursor stack = [N0, M1, F] at index=0 (top level) */
      let cursor = mk_cursor_at_index(~index=0, [n0, m1, f_frame]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      /* Full match: [N0, M1, F] == [N0, M1, F] → finds index 0 */
      check(
        bool,
        "should find N0 at index 0 via full-stack match",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "three-level: top probe unaffected by deeper info",
    `Quick,
    () => {
      let f_frame = frame(id_a);
      let f_frame2 = frame(id_b);
      let m1 = frame(id_c);
      let n0 = frame(id_e);
      let top_samples = [
        mk_sample(~seq=0, [f_frame]),
        mk_sample(~seq=1, [f_frame2]),
      ];
      /* Cursor stack = [N0, M1, F] at index=0 */
      let cursor = mk_cursor_at_index(~index=0, [n0, m1, f_frame]);
      let result =
        Sample.Selection.most_aligned_index(~ap_id=None, cursor, top_samples);
      /* Intermediate: [F] is suffix of [N0,M1,F] → picks F at index 0 */
      check(bool, "should find F at index 0", true, result == Some(0));
    },
  ),
  test_case(
    "three-level: most_aligned_sample picks correct mid sample",
    `Quick,
    () => {
      /* This tests the capture path (resolve_pending_probe_cursor).
       * When navigating from inner to mid, most_aligned_sample should
       * pick the mid sample in the same call branch. */
      let f_frame = frame(id_a);
      let m0 = frame(id_b);
      let m1 = frame(id_c);
      let m2 = frame(id_d);
      let n0 = frame(id_e);
      let mid_samples = [
        mk_sample(~seq=0, [m0, f_frame]),
        mk_sample(~seq=1, [m1, f_frame]),
        mk_sample(~seq=2, [m2, f_frame]),
      ];
      /* Cursor at inner level: [N0, M1, F] */
      let cursor = mk_cursor_at_index(~index=2, [n0, m1, f_frame]);
      let result =
        Sample.Selection.most_aligned_sample(
          ~ap_id=None,
          ~cursor,
          mid_samples,
        );
      switch (result) {
      | Some(s) =>
        check(int, "should pick M1 (seq=1), not M0 (seq=0)", 1, s.seq)
      | None => fail("expected Some sample")
      };
    },
  ),
  test_case(
    "three-level: capture preserves full stack through three levels",
    `Quick,
    () => {
      /* Simulates the full navigation: inner→mid→top.
       * Each step uses capture's is_suffix_of to preserve deeper info.
       * After reaching top, cursor should retain all three levels. */
      let f_frame = frame(id_a);
      let m1 = frame(id_c);
      let n0 = frame(id_e);
      /* Start: cursor at inner level */
      let cursor_inner: Sample.Focus.t = {
        call_stack: [n0, m1, f_frame],
        index: 2,
        pinned_stack: None,
        indicated_call: None,
        time: None,
        seq: 0,
        step_range: None,
        pending_focus: None,
      };
      /* Step 1: navigate to mid level. capture with [M1, F] */
      let mid_data: Sample.Capture.t = {
        call_stack: [m1, f_frame],
        time: 0.,
        seq: 0,
        step_start: 0,
        step_end: 0,
      };
      let is_suffix =
        Util_web.ListUtil.is_suffix_of(
          ~eq=CallStack.equal_frame,
          mid_data.call_stack,
          cursor_inner.call_stack,
        );
      check(bool, "[M1,F] is suffix of [N0,M1,F]", true, is_suffix);
      /* capture keeps deeper stack, lowers index */
      let cursor_mid: Sample.Focus.t = {
        ...cursor_inner,
        call_stack: is_suffix ? cursor_inner.call_stack : mid_data.call_stack,
        index: List.length(mid_data.call_stack) - 1,
      };
      check(int, "index should be 1 (mid level)", 1, cursor_mid.index);
      check(
        int,
        "stack should still be length 3",
        3,
        List.length(cursor_mid.call_stack),
      );
      /* Step 2: navigate to top level. capture with [F] */
      let top_data: Sample.Capture.t = {
        call_stack: [f_frame],
        time: 0.,
        seq: 0,
        step_start: 0,
        step_end: 0,
      };
      let is_suffix2 =
        Util_web.ListUtil.is_suffix_of(
          ~eq=CallStack.equal_frame,
          top_data.call_stack,
          cursor_mid.call_stack,
        );
      check(bool, "[F] is suffix of [N0,M1,F]", true, is_suffix2);
      let cursor_top: Sample.Focus.t = {
        ...cursor_mid,
        call_stack: is_suffix2 ? cursor_mid.call_stack : top_data.call_stack,
        index: List.length(top_data.call_stack) - 1,
      };
      check(int, "index should be 0 (top level)", 0, cursor_top.index);
      check(
        int,
        "stack should still be length 3",
        3,
        List.length(cursor_top.call_stack),
      );
      /* Now verify mid probe can recover M1 from this cursor */
      let m0 = frame(id_b);
      let m2 = frame(id_d);
      let mid_samples = [
        mk_sample(~seq=0, [m0, f_frame]),
        mk_sample(~seq=1, [m1, f_frame]),
        mk_sample(~seq=2, [m2, f_frame]),
      ];
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_top,
          mid_samples,
        );
      check(
        bool,
        "mid probe should find M1 at index 1",
        true,
        result == Some(1),
      );
    },
  ),
];

/* --- Tests: recursive functions (same call site ID at every depth) ---
 *
 * In recursive functions like factorial, every recursive call goes through
 * the same syntactic call site, so all stack frames share one ID. This
 * means every shallower stack is a suffix of every deeper stack:
 *   depth 1: [F]
 *   depth 2: [F, F]
 *   depth 3: [F, F, F]
 * [F] is a suffix of [F, F, F], [F, F] is a suffix of [F, F, F], etc.
 *
 * This is the scenario where effective_stack (cursor.index) matters:
 * the full call_stack can't discriminate between depths because all
 * depths share suffix relationships. The index encodes which depth
 * the user is currently looking at.
 *
 * Modeled after: let fact = fun ^^probe(x) -> ... fact(x-1) ... in fact(5)
 * Samples ordered by evaluation time: x=5, x=4, x=3, x=2, x=1 */

/* A single frame ID shared by all recursive calls */
let id_rec = Id.mk();
let f_rec = frame(id_rec);

/* Samples for a probe inside a recursive function called 5 times.
 * Each sample has a stack of repeated f_rec frames at increasing depth. */
let rec_samples = [
  mk_sample(~seq=0, [f_rec]), /* x=5, depth 1 */
  mk_sample(~seq=1, [f_rec, f_rec]), /* x=4, depth 2 */
  mk_sample(~seq=2, [f_rec, f_rec, f_rec]), /* x=3, depth 3 */
  mk_sample(~seq=3, [f_rec, f_rec, f_rec, f_rec]), /* x=2, depth 4 */
  mk_sample(~seq=4, [f_rec, f_rec, f_rec, f_rec, f_rec]) /* x=1, depth 5 */
];

let recursive_tests = [
  test_case(
    "recursive: click on shallower sample respects index",
    `Quick,
    () => {
      /* User is viewing x=3 (depth 3), clicks on x=4 (depth 2).
       * Write side: [F,F] is suffix of [F,F,F] → keep stack, lower index.
       * Cursor becomes (call_stack=[F,F,F], index=1).
       * Read side should find x=4 (depth 2), not snap back to x=3. */
      let cursor = mk_cursor_at_index(~index=1, [f_rec, f_rec, f_rec]);
      let result =
        Sample.Selection.most_aligned_index(~ap_id=None, cursor, rec_samples);
      check(
        bool,
        "should find depth-2 sample at index 1 (x=4), not depth-3 at index 2 (x=3)",
        true,
        result == Some(1),
      );
    },
  ),
  test_case(
    "recursive: click on shallowest sample respects index",
    `Quick,
    () => {
      /* User has been deep, clicks the shallowest sample (x=5, depth 1).
       * Write side preserves the deep stack.
       * Cursor: (call_stack=[F,F,F,F], index=0).
       * Read side should find x=5, not snap to x=2. */
      let cursor =
        mk_cursor_at_index(~index=0, [f_rec, f_rec, f_rec, f_rec]);
      let result =
        Sample.Selection.most_aligned_index(~ap_id=None, cursor, rec_samples);
      check(
        bool,
        "should find depth-1 sample at index 0 (x=5)",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "recursive: arrow left from x=3 to x=4 then arrow left again to x=5",
    `Quick,
    () => {
      /* Simulates two successive ArrowLeft presses in move_cursor.
       *
       * Step 1: At x=3 (depth 3). ArrowLeft captures x=4.
       *   capture: [F,F] suffix of [F,F,F] → keep. Cursor: stack=[F,F,F], index=1.
       *   most_aligned_index should return index 1 (x=4).
       *
       * Step 2: At x=4 (depth 2). ArrowLeft captures x=5.
       *   capture: [F] suffix of [F,F,F] → keep. Cursor: stack=[F,F,F], index=0.
       *   most_aligned_index should return index 0 (x=5). */

      /* Step 1: cursor after capturing x=4 */
      let cursor_at_x4 = mk_cursor_at_index(~index=1, [f_rec, f_rec, f_rec]);
      let idx_step1 =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_at_x4,
          rec_samples,
        );
      check(
        bool,
        "step 1: should be at index 1 (x=4)",
        true,
        idx_step1 == Some(1),
      );

      /* Step 2: ArrowLeft from index 1 → capture index 0 (x=5).
       * Cursor after capture: [F] is suffix of [F,F,F] → keep stack, index=0. */
      let cursor_at_x5 = mk_cursor_at_index(~index=0, [f_rec, f_rec, f_rec]);
      let idx_step2 =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_at_x5,
          rec_samples,
        );
      check(
        bool,
        "step 2: should be at index 0 (x=5)",
        true,
        idx_step2 == Some(0),
      );
    },
  ),
  test_case(
    "recursive: arrow right from x=3 to x=2, then arrow left back to x=3",
    `Quick,
    () => {
      /* Going deeper works (new stack replaces old), but coming back
       * must also work (shallower capture preserves deep stack).
       *
       * Step 1: At x=3. ArrowRight captures x=2.
       *   [F,F,F,F] NOT suffix of [F,F,F] → replace.
       *   Cursor: stack=[F,F,F,F], index=3.
       *
       * Step 2: At x=2. ArrowLeft captures x=3.
       *   [F,F,F] IS suffix of [F,F,F,F] → keep.
       *   Cursor: stack=[F,F,F,F], index=2. */

      /* Step 2 cursor (the interesting one): */
      let cursor_back_at_x3 =
        mk_cursor_at_index(~index=2, [f_rec, f_rec, f_rec, f_rec]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_back_at_x3,
          rec_samples,
        );
      check(
        bool,
        "should find x=3 at index 2, not x=2 at index 3",
        true,
        result == Some(2),
      );
    },
  ),
  test_case(
    "recursive: select Single returns correct sample after shallower click",
    `Quick,
    () => {
      /* End-to-end: Selection.select in Single mode should return the sample
       * at the user's active depth, not the deepest preserved depth. */
      let cursor = mk_cursor_at_index(~index=1, [f_rec, f_rec, f_rec]);
      let selected = do_select(~cursor, rec_samples);
      check(int, "should show 1 sample", 1, List.length(selected));
      switch (selected) {
      | [s] => check(int, "should be depth-2 sample (seq=1, x=4)", 1, s.seq)
      | _ => fail("expected exactly 1 sample")
      };
    },
  ),
  test_case(
    "recursive: cross-probe consistency after depth change",
    `Quick,
    () => {
      /* Two probes in a recursive function body. User changes depth at
       * probe B (ArrowLeft from depth 3 to depth 2). Navigate to probe A.
       * Probe A should also show depth 2, not snap back to depth 3.
       *
       * This tests that when cursor=(stack=[F,F,F], index=1), a DIFFERENT
       * probe (with the same recursive sample structure) also aligns
       * to depth 2. */
      let other_rec_samples = [
        mk_sample(~seq=10, [f_rec]),
        mk_sample(~seq=11, [f_rec, f_rec]),
        mk_sample(~seq=12, [f_rec, f_rec, f_rec]),
        mk_sample(~seq=13, [f_rec, f_rec, f_rec, f_rec]),
      ];
      let cursor = mk_cursor_at_index(~index=1, [f_rec, f_rec, f_rec]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          other_rec_samples,
        );
      check(
        bool,
        "other probe should also find depth-2 sample (index 1)",
        true,
        result == Some(1),
      );
    },
  ),
  test_case(
    "recursive: intent preserved when returning from non-recursive probe",
    `Quick,
    () => {
      /* User is at depth 3 in a recursive probe. Navigates to a
       * non-recursive probe (e.g. the test expression ^^probe(fact(5))),
       * which has a single sample with empty/different call stack.
       * Navigate back. The recursive probe should recover depth 3.
       *
       * After navigating away: cursor.index = -1 or 0, but call_stack
       * preserved as [F,F,F]. When returning, effective_stack is too
       * shallow to match any recursive sample, so the full stack should
       * kick in and recover the depth-3 selection. */
      let cursor_after_return =
        mk_cursor_at_index(~index=-1, [f_rec, f_rec, f_rec]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_after_return,
          rec_samples,
        );
      check(
        bool,
        "should recover depth-3 sample (index 2) via full stack",
        true,
        result == Some(2),
      );
    },
  ),
];

/* --- Test: call-click alignment ---
 *
 * Scenario: function f called 3 times at top level.
 * Probe inside f on parameter x: 3 samples with call_stacks [A], [B], [C].
 * Probes on f(1), f(2), f(3): each has 1 top-level sample (call_stack=[]).
 *
 * When user clicks on the probe for f(2), in Single mode the inner probe
 * should show the sample from that call. The click should set:
 *   cursor = {call_stack: [], index: -1, indicated_call: Some(app_id_f2)}
 *
 * Selection logic: tiers 1a/1b (suffix scan) fail because cursor stack is [].
 * Tier 2 (is_call_cursor) fails. Tier 3 (is_below_indicated_call) should
 * match the sample whose call_stack contains the indicated app ID. */

let call_click_alignment_tests = [
  test_case(
    "call-click: indicated_call aligns inner probe via tier 3",
    `Quick,
    () => {
      let id_app_1 = Id.mk();
      let id_app_2 = Id.mk();
      let id_app_3 = Id.mk();
      /* Inner probe samples: one per call to f */
      let inner_samples = [
        mk_sample(~seq=0, [named_frame(id_app_1, "f")]),
        mk_sample(~seq=1, [named_frame(id_app_2, "f")]),
        mk_sample(~seq=2, [named_frame(id_app_3, "f")]),
      ];
      /* Cursor after clicking call probe for f(2):
       * capture({call_stack:[], ...}, Some(id_app_2))
       * → {call_stack: [], index: -1, indicated_call: Some(id_app_2)} */
      let cursor = mk_cursor(~indicated_call=Some(id_app_2), []);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "should find sample from f(2) at index 1",
        true,
        result == Some(1),
      );
    },
  ),
  test_case(
    "call-click: select Single shows correct inner sample via indicated_call",
    `Quick,
    () => {
      let id_app_1 = Id.mk();
      let id_app_2 = Id.mk();
      let id_app_3 = Id.mk();
      let inner_samples = [
        mk_sample(~seq=0, [named_frame(id_app_1, "f")]),
        mk_sample(~seq=1, [named_frame(id_app_2, "f")]),
        mk_sample(~seq=2, [named_frame(id_app_3, "f")]),
      ];
      let cursor = mk_cursor(~indicated_call=Some(id_app_2), []);
      let selected = do_select(~cursor, inner_samples);
      check(int, "should show 1 sample", 1, List.length(selected));
      switch (selected) {
      | [s] => check(int, "should be sample from f(2) (seq=1)", 1, s.seq)
      | _ => fail("expected exactly 1 sample")
      };
    },
  ),
  test_case(
    "call-click: each call aligns to its own inner sample",
    `Quick,
    () => {
      let id_app_1 = Id.mk();
      let id_app_2 = Id.mk();
      let id_app_3 = Id.mk();
      let inner_samples = [
        mk_sample(~seq=0, [named_frame(id_app_1, "f")]),
        mk_sample(~seq=1, [named_frame(id_app_2, "f")]),
        mk_sample(~seq=2, [named_frame(id_app_3, "f")]),
      ];
      /* Click f(1) */
      let cursor_1 = mk_cursor(~indicated_call=Some(id_app_1), []);
      let result_1 =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_1,
          inner_samples,
        );
      check(bool, "f(1) should align to index 0", true, result_1 == Some(0));
      /* Click f(2) */
      let cursor_2 = mk_cursor(~indicated_call=Some(id_app_2), []);
      let result_2 =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_2,
          inner_samples,
        );
      check(bool, "f(2) should align to index 1", true, result_2 == Some(1));
      /* Click f(3) */
      let cursor_3 = mk_cursor(~indicated_call=Some(id_app_3), []);
      let result_3 =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor_3,
          inner_samples,
        );
      check(bool, "f(3) should align to index 2", true, result_3 == Some(2));
    },
  ),
  test_case(
    "call-click: WITHOUT indicated_call, always picks first (no discrimination)",
    `Quick,
    () => {
      /* When indicated_call is None (e.g. ap_id not set on call probe),
       * tier 3 can't fire. The fallback (is_related) finds the first
       * Below sample, which is always index 0 regardless of which call
       * was clicked. This test documents the limitation. */
      let id_app_1 = Id.mk();
      let id_app_2 = Id.mk();
      let id_app_3 = Id.mk();
      let inner_samples = [
        mk_sample(~seq=0, [named_frame(id_app_1, "f")]),
        mk_sample(~seq=1, [named_frame(id_app_2, "f")]),
        mk_sample(~seq=2, [named_frame(id_app_3, "f")]),
      ];
      /* Cursor with NO indicated_call — as if click didn't propagate ap_id */
      let cursor = mk_cursor([]);
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      /* Falls through to is_related → always finds first sample */
      check(
        bool,
        "without indicated_call, always picks index 0 (first sample)",
        true,
        result == Some(0),
      );
    },
  ),
];

/* --- Test: perspective extension (app probe click) ---
 *
 * Perspective extension is when clicking an app probe prepends the app's
 * ID as a frame to the call_stack, with index = List.length(data.call_stack) - 1.
 * This means the extended frame is always below the index (ghosted in the UI).
 * We test this by constructing cursors that represent the post-extension state
 * and verifying alignment behavior. */

let perspective_extension_tests = [
  test_case(
    "extension: app probe at top level creates ghost frame",
    `Quick,
    () => {
      /* Simulate: clicking celsius(72.5) at top level.
       * data.call_stack was [], so index = len([]) - 1 = -1.
       * The extension prepends frame(id_a) to the call_stack.
       * effective_stack at index=-1 is empty → tier 1a finds nothing.
       * Tier 1b scans the full stack [frame(id_a)] and finds the suffix match. */
      let cursor = mk_cursor_at_index(~index=-1, [frame(id_a)]);
      let inner_samples = [
        mk_sample(~seq=0, [named_frame(id_a, "celsius")]),
      ];
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "should find sample via full-sightline fallback (tier 1b)",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "extension: app probe inside a function creates ghost frame",
    `Quick,
    () => {
      /* Simulate: inside outer (index=0), clicking inner(x*2) app probe.
       * data.call_stack was [frame(id_a)], so index = len([frame(id_a)]) - 1 = 0.
       * Extension prepends id_b → call_stack = [frame(id_b), frame(id_a)].
       * effective_stack at index=0 is [frame(id_a)] (the outer frame only).
       * Tier 1a: [frame(id_a)] doesn't suffix-match a 2-element sample → nothing.
       * Tier 1b: full stack [frame(id_b), frame(id_a)] matches the sample. */
      let cursor =
        mk_cursor_at_index(~index=0, [frame(id_b), frame(id_a)]);
      let inner_samples = [
        mk_sample(
          ~seq=0,
          [named_frame(id_b, "inner"), named_frame(id_a, "outer")],
        ),
      ];
      let result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          cursor,
          inner_samples,
        );
      check(
        bool,
        "should find sample via full-sightline fallback (tier 1b)",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "extension: peek then navigate in produces same alignment",
    `Quick,
    () => {
      /* Peeking (after extension) and being-there should both find the sample.
       * Peek cursor: call_stack = [frame(id_a)], index = -1
       *   → effective_stack = [], tier 1a: nothing. Tier 1b: [frame(id_a)] matches.
       * Navigate-in cursor: call_stack = [frame(id_a)], index = 0
       *   → effective_stack = [frame(id_a)], tier 1a: suffix match directly. */
      let samples = [mk_sample(~seq=0, [named_frame(id_a, "celsius")])];
      let peek_cursor = mk_cursor_at_index(~index=-1, [frame(id_a)]);
      let navigate_cursor = mk_cursor_at_index(~index=0, [frame(id_a)]);
      let peek_result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          peek_cursor,
          samples,
        );
      let nav_result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          navigate_cursor,
          samples,
        );
      check(
        bool,
        "peek should find sample (index 0)",
        true,
        peek_result == Some(0),
      );
      check(
        bool,
        "navigate-in should find same sample (index 0)",
        true,
        nav_result == Some(0),
      );
    },
  ),
  test_case(
    "extension: multiple inner samples, extension picks correct branch",
    `Quick,
    () => {
      /* Simulate: top-level with two app probes (celsius and fahrenheit).
       * Extension for celsius: call_stack = [frame(id_a)], index = -1.
       * Samples inside the function body have different call stack frames.
       * Tier 1b should pick the sample whose stack matches the extended frame. */
      let cursor = mk_cursor_at_index(~index=-1, [frame(id_a)]);
      let samples = [
        mk_sample(~seq=0, [named_frame(id_a, "celsius")]),
        mk_sample(~seq=1, [named_frame(id_b, "fahrenheit")]),
      ];
      let result =
        Sample.Selection.most_aligned_index(~ap_id=None, cursor, samples);
      check(
        bool,
        "should pick celsius (index 0), not fahrenheit (index 1)",
        true,
        result == Some(0),
      );
    },
  ),
  test_case(
    "extension: suffix preservation after peek",
    `Quick,
    () => {
      /* After peeking at celsius (extension), the inner sample's stack
       * [frame(id_a)] IS a suffix of cursor's [frame(id_a)].
       * After navigating to the inner sample, the cursor would become
       * index=0 (same stack, higher index).
       * Both states should give consistent results. */
      let samples = [mk_sample(~seq=0, [named_frame(id_a, "celsius")])];
      /* Extended cursor (peeking) */
      let extended_cursor = mk_cursor_at_index(~index=-1, [frame(id_a)]);
      let ext_result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          extended_cursor,
          samples,
        );
      /* After navigating in (same stack, index bumped up) */
      let navigated_cursor = mk_cursor_at_index(~index=0, [frame(id_a)]);
      let nav_result =
        Sample.Selection.most_aligned_index(
          ~ap_id=None,
          navigated_cursor,
          samples,
        );
      check(
        bool,
        "extended cursor should find sample",
        true,
        ext_result == Some(0),
      );
      check(
        bool,
        "navigated cursor should find same sample",
        true,
        nav_result == Some(0),
      );
    },
  ),
];

let tests = (
  "SampleSelection",
  List.concat([
    equality_tests,
    relation_tests,
    pin_tests,
    select_tests,
    is_same_call_tests,
    closest_tests,
    empty_status_tests,
    intent_preservation_tests,
    three_level_tests,
    recursive_tests,
    call_click_alignment_tests,
    perspective_extension_tests,
  ]),
);
