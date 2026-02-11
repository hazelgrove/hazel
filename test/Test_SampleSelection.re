open Alcotest;
open Language;

/**
 * Unit tests for Sample.Selection and Sample.Cursor logic.
 *
 * These test the pure functions that determine which samples are visible
 * given a cursor position, pin state, and display mode. No evaluation needed.
 *
 * The key pipeline tested:
 *   samples -> filter_by_pin -> first_related_index/closest_to_cursor -> select
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
};

/* Make a named stack frame (as evaluator produces) */
let named_frame = (id: Id.t, name: string): Sample.stack_frame => {
  id,
  name: Some(name),
};

/* Fixed IDs for readable tests */
let id_a = Id.mk();
let id_b = Id.mk();
let id_c = Id.mk();
let id_d = Id.mk();

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

/* --- Test: closest_to_cursor --- */

let closest_tests = [
  test_case(
    "closest_to_cursor: exact match preferred",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let exact = mk_sample(~seq=0, [frame(id_a)]);
      let related = mk_sample(~seq=1, [frame(id_b), frame(id_a)]);
      let result =
        Sample.Selection.closest_to_cursor(
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
    "closest_to_cursor: None vs Some names (regression)",
    `Quick,
    () => {
      let cursor = mk_cursor([frame(id_a)]);
      let sample = mk_sample(~seq=42, [named_frame(id_a, "f")]);
      let result =
        Sample.Selection.closest_to_cursor(~ap_id=None, ~cursor, [sample]);
      switch (result) {
      | Some(s) => check(int, "should find the sample", 42, s.seq)
      | None => fail("should find a sample (was failing before fix)")
      };
    },
  ),
  test_case(
    "closest_to_cursor: falls back to best suffix match",
    `Quick,
    () => {
      /* Cursor at [C, A], samples at [B, A] and [D].
       * [B, A] shares suffix [A] with cursor, [D] shares nothing.
       * closest_to_cursor should prefer [B, A]. */
      let cursor = mk_cursor([frame(id_c), frame(id_a)]);
      let s_suffix = mk_sample(~seq=1, [frame(id_b), frame(id_a)]);
      let s_none = mk_sample(~seq=2, [frame(id_d)]);
      let result =
        Sample.Selection.closest_to_cursor(
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
  ]),
);
