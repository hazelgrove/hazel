open Alcotest;
open Language;

/**
 * Unit tests for Sample.Selection and Sample.Cursor logic.
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
let frame = (~name=None, id: Id.t): Sample.stack_frame => {
  id,
  name,
  fn_def_id: None,
};

/* Make a named stack frame (as evaluator produces) */
let named_frame = (id: Id.t, name: string): Sample.stack_frame => {
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
let id_g = Id.mk();

/* Make a minimal sample with the given call stack */
let mk_sample =
    (~seq=0, ~step_start=0, ~step_end=0, stack: Sample.call_stack): Sample.t => {
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
      stack: Sample.call_stack,
    )
    : Sample.Cursor.t => {
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
      ~cursor: Sample.Cursor.t,
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
      ~cursor: Sample.Cursor.t,
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
      check(bool, "should be equal", true, Sample.equal_stack_frame(f1, f2));
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
        Sample.equal_stack_frame(f1, f2),
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
        Sample.equal_call_stack(cursor_stack, eval_stack),
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
        Sample.Cursor.relation(~trimmed=false, ~ap_id=None, cursor, sample);
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
      stack: Sample.call_stack,
    )
    : Sample.Cursor.t => {
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
      let cursor_inner: Sample.Cursor.t = {
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
        Util.ListUtil.is_suffix_of(
          ~eq=Sample.equal_stack_frame,
          mid_data.call_stack,
          cursor_inner.call_stack,
        );
      check(bool, "[M1,F] is suffix of [N0,M1,F]", true, is_suffix);
      /* capture keeps deeper stack, lowers index */
      let cursor_mid: Sample.Cursor.t = {
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
        Util.ListUtil.is_suffix_of(
          ~eq=Sample.equal_stack_frame,
          top_data.call_stack,
          cursor_mid.call_stack,
        );
      check(bool, "[F] is suffix of [N0,M1,F]", true, is_suffix2);
      let cursor_top: Sample.Cursor.t = {
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
  ]),
);
