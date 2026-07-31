open Alcotest;
open Language;

/**
 * Tests for the pin-gated arrow-nav reachability rule:
 *   Sample.Selection.is_reachable_pinned
 *
 * See ProbeProj.is_cursor_aligned for the design rationale (short form
 * reproduced there). These tests assert the rule at the pure-function
 * level: given a program, a cursor sample, and a pin, which other probes
 * are arrow-reachable?
 *
 * Fixture shape:
 *   - A program with manual ^^probe(...) markers.
 *   - The "focus" probe: which sample we pretend the cursor is on.
 *   - The "pin head ap id" probe: the application whose sample we pin.
 *   - A list of "reachable" probe tags and "unreachable" probe tags.
 *
 * Probes are referred to by a stable tag extracted from the source
 * (the text of the probed expression), to avoid depending on evaluation
 * order or internal ids.
 */

/* These tests exercise the rule's corner cases with hand-crafted
 * call stacks, so they're fast and precise and don't depend on
 * evaluation order or probe-labelling infrastructure. An end-to-end
 * evaluate-and-check suite can be added later once the fixture
 * format and probe-label mapping is stabilized. */

let frame = (id: Id.t, fn_def_id: option(Id.t)): CallStack.frame => {
  id,
  name: None,
  fn_def_id,
};

let mk_sample =
    (~seq=0, ~step_start=0, ~step_end=0, stack: CallStack.t): Sample.t => {
  id: Hashtbl.hash((stack, Id.invalid)),
  syntax_id: Id.invalid,
  value: IdTagged.FreshGrammar.Exp.empty_hole(),
  env: Sample.Env.empty,
  call_stack: stack,
  args: None,
  frame: None,
  time: 0.0,
  seq,
  origin: Probe,
  step_start,
  step_end,
};

let mk_focus = (~pinned=None, stack: CallStack.t): Sample.Focus.t => {
  call_stack: stack,
  index: List.length(stack) - 1,
  pinned_stack: pinned,
  indicated_call: None,
  time: None,
  seq: 0,
  step_range: None,
  pending_focus: None,
};

/* Fixed ids for readable tests */
let id_foo = Id.mk();
let id_double = Id.mk();
let id_bar = Id.mk();
let id_ap_foo = Id.mk();
let id_ap_double = Id.mk();
let id_ap_bar = Id.mk();
let id_ap_r = Id.mk(); /* recursive self-call ap */

let unit_tests = [
  test_case(
    "rule (a): exact match same invocation — reachable",
    `Quick,
    () => {
      let cursor_stack = [frame(id_ap_foo, Some(id_foo))];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target = mk_sample(cursor_stack);
      check(
        bool,
        "same-invocation sibling is reachable",
        true,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "rule (a): same id, different stack depth — not reachable",
    `Quick,
    () => {
      /* Cursor is inside foo (1 frame). Target is inside foo-inside-bar
       * (2 frames, sharing the foo frame). Same call_stack value? No,
       * they differ in length. Rule (a) fails. */
      let cursor_stack = [frame(id_ap_foo, Some(id_foo))];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target =
        mk_sample([
          frame(id_ap_foo, Some(id_foo)),
          frame(id_ap_bar, Some(id_bar)),
        ]);
      /* Same target_fn (foo) as cursor_fn? innermost of target =
       * id_ap_foo frame with fn_def_id=foo. innermost of cursor =
       * same. Same fn. Rule (b) blocked. Rule (a) not equal. */
      check(
        bool,
        "different-depth same-fn sibling is blocked",
        false,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "rule (b) walk up: ancestor in different fn body — reachable",
    `Quick,
    () => {
      /* Cursor is inside double (called from foo). Target sample is
       * captured in foo's body (shallower stack). Walk up works. */
      let foo_frame = frame(id_ap_foo, Some(id_foo));
      let double_frame = frame(id_ap_double, Some(id_double));
      let cursor_stack = [double_frame, foo_frame];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target = mk_sample([foo_frame]);
      check(
        bool,
        "foo-body sample is reachable from inside double",
        true,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "rule (b) walk down: descendant in different fn body — reachable",
    `Quick,
    () => {
      /* Cursor is inside foo. Target sample is deeper (inside double
       * called from foo). Walking "back down" works. */
      let foo_frame = frame(id_ap_foo, Some(id_foo));
      let double_frame = frame(id_ap_double, Some(id_double));
      let cursor_stack = [foo_frame];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target = mk_sample([double_frame, foo_frame]);
      check(
        bool,
        "double-body sample is reachable from inside foo",
        true,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "recursion compromise: same-fn deeper recursion — not reachable",
    `Quick,
    () => {
      /* Cursor at recursion depth 2 of foo. Target sample at depth 3
       * (one more recursive call). Same fn_def_id — blocked by rule (b)
       * same-fn check. Rule (a) not equal (different lengths). */
      let f = frame(id_ap_r, Some(id_foo));
      let cursor_stack = [f, f];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target = mk_sample([f, f, f]);
      check(
        bool,
        "same-fn deeper recursive sample is blocked",
        false,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "recursion compromise: same-fn shallower recursion — not reachable",
    `Quick,
    () => {
      /* Reverse: cursor at depth 3, target at depth 2. Still same-fn,
       * still blocked. */
      let f = frame(id_ap_r, Some(id_foo));
      let cursor_stack = [f, f, f];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      let target = mk_sample([f, f]);
      check(
        bool,
        "same-fn shallower recursive sample is blocked",
        false,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
  test_case(
    "empty target samples — not reachable",
    `Quick,
    () => {
      let cursor_stack = [frame(id_ap_foo, Some(id_foo))];
      let cursor = mk_focus(~pinned=Some(cursor_stack), cursor_stack);
      check(
        bool,
        "empty sample list means unreachable",
        false,
        Sample.Selection.is_reachable_pinned(~cursor, []),
      );
    },
  ),
  test_case(
    "top-level cursor + top-level target — reachable via rule (a)",
    `Quick,
    () => {
      let cursor = mk_focus(~pinned=Some([]), []);
      let target = mk_sample([]);
      check(
        bool,
        "two top-level probes reach each other",
        true,
        Sample.Selection.is_reachable_pinned(~cursor, [target]),
      );
    },
  ),
];

let tests = ("ProbeNav.ReachabilityRule", unit_tests);
