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

/* ---- Scaffold generation tests ---- */

let generation_tests = (
  "TyDiScaffold.Generation",
  [
    /* Grout to right: scaffold uses ○, format → display f(○, ?) */
    scaffold_test(
      ~name="2-arg function: empty hole after open paren",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Convex left (no grout): scaffold uses , ○ format → display f(1, ○) */
    scaffold_test(
      ~name="2-arg function: after first arg",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Grout to right: g(○, ○, ?) */
    scaffold_test(
      ~name="3-arg function: empty hole after open paren",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(¦",
      ~expect=Some(hole_char ++ ", " ++ hole_char ++ ", "),
    ),
    /* Grout to right after comma: g(1, ○, ?) */
    scaffold_test(
      ~name="3-arg function: one comma already present",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Single-value match suppression: f(p▎ where p satisfies the whole Prod.
     * NOTE: This test passes but suppression does NOT work in the editor.
     * The editor's zipper state differs from the test harness in some way
     * that causes the suppression check to fail. Needs investigation. */
    scaffold_test(
      ~name="Suppress when value already matches full Prod type",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let p : (Int, String) = (1, \"a\") in f(p¦",
      ~expect=None,
    ),
    /* Grout to right: (○, ?) */
    scaffold_test(
      ~name="Parenthesized tuple: let binding",
      ~code="let t : (Int, Bool) = (¦",
      ~expect=Some(hole_char ++ ", "),
    ),
    /* Bare tuple: no scaffold (only inside parens) */
    scaffold_test(
      ~name="No scaffold for bare tuple (no parens)",
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

/* ---- Tab acceptance tests ---- */

let acceptance_tests = (
  "TyDiScaffold.Acceptance",
  [
    /* f(1▎ with scaffold → Tab → f(1,¦? */
    accept_test(
      ~name="2-arg: Tab inserts comma, caret on hole",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in f(1,¦?",
    ),
    /* g(1▎ with scaffold → Tab → g(1,¦? (one comma per Tab) */
    accept_test(
      ~name="3-arg: Tab inserts one comma",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦",
      ~goal="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1,¦?",
    ),
  ],
);

let tests = [generation_tests, acceptance_tests];
