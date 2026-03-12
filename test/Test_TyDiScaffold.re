open Alcotest;
open Haz3lcore;
open Language;

/* Unicode circle used as hole placeholder in scaffold display strings */
let hole_char = "○"; /* U+25CB */

/* Build a zipper from code string with caret position (¦),
 * compute statics, and return the scaffold buffer display string.
 *
 * The scaffold system generates a buffer like ", ○" when the caret
 * is inside parentheses and the expected type is a Prod (tuple). */
let scaffold_suggest = (code: string): option(string) => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = TyDi.set_scaffold(~info_map, z);
  TyDi.get_unparsed_buffer(z);
};

let scaffold_test = (~name, ~code, ~expect) =>
  test_case(name, `Quick, () =>
    check(option(string), name, expect, scaffold_suggest(code))
  );

/* Print the zipper state after accepting the scaffold buffer via Tab.
 * Uses the same printer as editing tests (? for holes, ¦ for caret). */
let scaffold_accept = (code: string): string => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = TyDi.set_scaffold(~info_map, z);
  /* Accept the buffer (Tab) */
  let z = Test_Editing.perform(z, [Action.Buffer(Accept)]);
  Test_Editing.printer(z);
};

let accept_test = (~name, ~code, ~goal) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      scaffold_accept(code),
    )
  );

/* Get the ana type at the caret position after scaffold virtual insertion.
 * This tests that statics sees the tuple structure through the scaffold. */
let scaffold_ana = (code: string): option(Typ.t) => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  /* First pass: compute statics to get ci for scaffold generation */
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = TyDi.set_scaffold(~info_map, z);
  /* Second pass: reify scaffold into zipper, then dump for statics */
  let z_reified = TyDi.reify_scaffold(z);
  let term = MakeTerm.from_zip_for_sem(z_reified).term;
  let info_map2 =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  /* Look up ana at the indicated piece */
  switch (Indicated.ci_of(z, info_map2)) {
  | Some(InfoExp({ana, _})) => Some(ana)
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };
};

/* ---- Scaffold generation: shard case (only open paren placed) ---- */

let shard_tests = (
  "TyDiScaffold.Shard",
  [
    /* Grout to right: f(○, ?) */
    scaffold_test(
      ~name="2-arg: empty hole after open paren",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Convex left (no grout): f(1, ○) */
    scaffold_test(
      ~name="2-arg: after first arg",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Grout to right: g(○, ○, ?) */
    scaffold_test(
      ~name="3-arg: empty hole after open paren",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(¦",
      ~expect=Some(hole_char ++ ", " ++ hole_char ++ ", "),
    ),
    /* g(1, ○, ?) */
    scaffold_test(
      ~name="3-arg: one comma already present",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Suppression: f(p▎ where p satisfies the whole Prod */
    scaffold_test(
      ~name="Suppress: value matches full Prod type",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let p : (Int, String) = (1, \"a\") in f(p¦",
      ~expect=None,
    ),
    /* Explicit parens: (○, ?) */
    scaffold_test(
      ~name="Explicit parens: let binding",
      ~code="let t : (Int, Bool) = (¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Bare tuple: no scaffold */
    scaffold_test(
      ~name="No scaffold for bare tuple",
      ~code="let t : (Int, Bool) = 1¦",
      ~expect=None,
    ),
    /* Non-Prod type: no scaffold */
    scaffold_test(
      ~name="No scaffold for non-Prod type",
      ~code="let f : Int -> Bool = fun x -> true in f(1¦",
      ~expect=None,
    ),
    /* All commas already present: no scaffold */
    scaffold_test(
      ~name="No scaffold when all commas present",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1, ¦",
      ~expect=None,
    ),
  ],
);

/* ---- Scaffold generation: ancestor case (both parens placed) ---- */

let ancestor_tests = (
  "TyDiScaffold.Ancestor",
  [
    /* f(1¦) — both parens, caret after first arg */
    scaffold_test(
      ~name="2-arg: after first arg, both parens",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦)",
      ~expect=Some(", " ++ hole_char),
    ),
    /* g(1¦) — 3-arg, both parens */
    scaffold_test(
      ~name="3-arg: after first arg, both parens",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦)",
      ~expect=Some(", " ++ hole_char ++ ", " ++ hole_char),
    ),
    /* g(1, 2¦) — 3-arg, one comma present */
    scaffold_test(
      ~name="3-arg: one comma, both parens",
      ~code=
        "let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, 2¦)",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Explicit parens: let t : (Int, Bool) = (1¦) */
    scaffold_test(
      ~name="Explicit parens: let binding, both parens",
      ~code="let t : (Int, Bool) = (1¦) in t",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Suppression with both parens: f(p¦) where p matches Prod */
    scaffold_test(
      ~name="Suppress: value matches Prod, both parens",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let p : (Int, String) = (1, \"a\") in f(p¦)",
      ~expect=None,
    ),
    /* All commas present with both parens: no scaffold */
    scaffold_test(
      ~name="No scaffold when all commas present, both parens",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in f(1, 2¦)",
      ~expect=None,
    ),
  ],
);

/* ---- Scaffold generation: mid-expression (trailing code) ---- */

let midexpr_tests = (
  "TyDiScaffold.MidExpr",
  [
    /* f(1¦) + 2 — scaffold inside expression with trailing code */
    scaffold_test(
      ~name="Trailing addition",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in f(1¦) + 2",
      ~expect=Some(", " ++ hole_char),
    ),
    /* let result = f(1¦) in result — inside let binding body */
    scaffold_test(
      ~name="Inside let binding body",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let r = f(1¦) in r",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Nested call: h(f(1¦)) — scaffold on inner function */
    scaffold_test(
      ~name="Nested function call: inner scaffold",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let h : Int -> Int = fun y -> y in h(f(1¦))",
      ~expect=Some(", " ++ hole_char),
    ),
  ],
);

/* ---- Scaffold generation: pattern position (Phase 7) ---- */

/* Phase 7: Pattern scaffolds need work. The caret ends up at Inner
 * position on the `(` delimiter rather than Outer inside the child,
 * so set_scaffold bails early. Uncomment when Phase 7 is implemented.
 *
 * let pattern_tests = (
 *   "TyDiScaffold.Pattern",
 *   [
 *     scaffold_test(
 *       ~name="Pattern: let binding with tuple annotation",
 *       ~code="let (¦) : (Int, Bool) = (1, true) in 0",
 *       ~expect=Some(hole_char ++ ", "),
 *     ),
 *   ],
 * );
 */

/* ---- Tab acceptance tests ---- */

let acceptance_tests = (
  "TyDiScaffold.Acceptance",
  [
    /* Shard case: f(1▎ → Tab → f(1,¦? */
    accept_test(
      ~name="Shard: 2-arg Tab inserts comma",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in f(1,¦?",
    ),
    /* Shard case: g(1▎ → Tab → g(1,¦? */
    accept_test(
      ~name="Shard: 3-arg Tab inserts one comma",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦",
      ~goal="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1,¦?",
    ),
  ],
);

let tests = [
  shard_tests,
  ancestor_tests,
  midexpr_tests,
  acceptance_tests,
];
