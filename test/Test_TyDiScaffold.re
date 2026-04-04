open Alcotest;
open Haz3lcore;
open Language;

/* Build zipper, compute statics, run set_assist_buffer, return the
 * buffer display string (if any). This tests the full combined path. */
let assist_suggest = (code: string): option(string) => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = Buffer.set_assist_buffer(~info_map, z);
  TyDi.get_unparsed_buffer(z);
};

let scaffold_test = (~name, ~code, ~expect) =>
  test_case(name, `Quick, () =>
    check(option(string), name, expect, assist_suggest(code))
  );

/* Print the zipper state after accepting the scaffold buffer via Tab.
 * Uses the same printer as editing tests (? for holes, ¦ for caret). */
let scaffold_accept = (code: string): string => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = Buffer.set_assist_buffer(~info_map, z);
  /* Accept the buffer (Tab) */
  let z = Test_Editing.perform(z, [Action.Buffer(Accept)]);
  Test_Editing.printer(z);
};

/* Accept scaffold, recompute statics + scaffold, accept again.
 * Simulates multiple Tab presses with scaffold regeneration between. */
let multi_accept = (code: string, n_tabs: int): string => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let rec go = (z, remaining) =>
    if (remaining <= 0) {
      z;
    } else {
      let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
      let info_map =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
      let z = Buffer.set_assist_buffer(~info_map, z);
      let z = Test_Editing.perform(z, [Action.Buffer(Accept)]);
      go(z, remaining - 1);
    };
  Test_Editing.printer(go(z, n_tabs));
};

let multi_accept_test = (~name, ~code, ~n_tabs, ~goal) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      multi_accept(code, n_tabs),
    )
  );

let accept_test = (~name, ~code, ~goal) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      scaffold_accept(code),
    )
  );

/* ---- Scaffold generation: shard case (only open paren placed) ---- */

let shard_tests = (
  "TyDiScaffold.Shard",
  [
    /* Grout to right: f(?, ?) */
    scaffold_test(
      ~name="2-arg: empty hole after open paren",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(¦",
      ~expect=Some("?, "),
    ),
    /* Convex left (no grout): f(1, ?) */
    scaffold_test(
      ~name="2-arg: after first arg",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~expect=Some(", ?"),
    ),
    /* Grout to right: g(?, ?, ?) */
    scaffold_test(
      ~name="3-arg: empty hole after open paren",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(¦",
      ~expect=Some("?, ?, "),
    ),
    /* g(1, ?, ?) */
    scaffold_test(
      ~name="3-arg: one comma already present",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦",
      ~expect=Some("?, "),
    ),
    /* Explicit parens: (?, ?) */
    scaffold_test(
      ~name="Explicit parens: let binding",
      ~code="let t : (Int, Bool) = (¦",
      ~expect=Some("?, "),
    ),
    /* Type alias: type Toop = (Int, Bool) in let x: Toop = (¦ */
    scaffold_test(
      ~name="Type alias: let binding with alias",
      ~code="type Toop = (Int, Bool) in let x : Toop = (¦",
      ~expect=Some("?, "),
    ),
    /* Type alias with labels */
    scaffold_test(
      ~name="Type alias: labeled tuple alias",
      ~code="type Toop = (zoo=Bool, yoop=(Int, String)) in let x : Toop = (¦",
      ~expect=Some("zoo=?, "),
    ),
    /* Type alias: after first arg */
    scaffold_test(
      ~name="Type alias: after first arg",
      ~code="type Toop = (Int, Bool) in let x : Toop = (1¦",
      ~expect=Some(", ?"),
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
      ~expect=Some(", ?"),
    ),
    /* g(1¦) — 3-arg, both parens */
    scaffold_test(
      ~name="3-arg: after first arg, both parens",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦)",
      ~expect=Some(", ?, ?"),
    ),
    /* g(1, 2¦) — 3-arg, one comma present */
    scaffold_test(
      ~name="3-arg: one comma, both parens",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, 2¦)",
      ~expect=Some(", ?"),
    ),
    /* Explicit parens: let t : (Int, Bool) = (1¦) */
    scaffold_test(
      ~name="Explicit parens: let binding, both parens",
      ~code="let t : (Int, Bool) = (1¦) in t",
      ~expect=Some(", ?"),
    ),
    /* Type alias with both parens */
    scaffold_test(
      ~name="Type alias: both parens",
      ~code="type Toop = (Int, Bool) in let x : Toop = (1¦) in x",
      ~expect=Some(", ?"),
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
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1, 2¦)",
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
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦) + 2",
      ~expect=Some(", ?"),
    ),
    /* let result = f(1¦) in result — inside let binding body */
    scaffold_test(
      ~name="Inside let binding body",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let r = f(1¦) in r",
      ~expect=Some(", ?"),
    ),
    /* Nested call: h(f(1¦)) — scaffold on inner function */
    scaffold_test(
      ~name="Nested function call: inner scaffold",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let h : Int -> Int = fun y -> y in h(f(1¦))",
      ~expect=Some(", ?"),
    ),
  ],
);

/* ---- Scaffold generation: nested tuples (Phase 5) ---- */

let nested_tests = (
  "TyDiScaffold.Nested",
  [
    /* f expects ((Int, Int), String). After typing first nested tuple,
     * scaffold should show remaining outer element: `, ?` */
    scaffold_test(
      ~name="Nested tuple: after inner tuple value",
      ~code=
        "let f : ((Int, Int), String) -> Bool = fun x -> true in f((1, 2)¦)",
      ~expect=Some(", ?"),
    ),
    /* f expects (Int, (String, Bool)). After typing first element,
     * scaffold should show `, ?` for the remaining element */
    scaffold_test(
      ~name="Nested tuple: second element is tuple",
      ~code="let f : (Int, (String, Bool)) -> Bool = fun x -> true in f(1¦)",
      ~expect=Some(", ?"),
    ),
    /* Genuine match: (1, "a") matches (Int, String) — should suppress */
    scaffold_test(
      ~name="Nested: genuine match suppresses scaffold",
      ~code="let f : (Int, String) -> Bool = fun x -> true in f((1, \"a\")¦)",
      ~expect=None,
    ),
    /* 3-element nested: ((Int,Int), String, Bool) with inner tuple typed */
    scaffold_test(
      ~name="Nested 3-elem: after inner tuple",
      ~code=
        "let f : ((Int, Int), String, Bool) -> Int = fun x -> 0 in f((1, 2)¦)",
      ~expect=Some(", ?, ?"),
    ),
    /* --- Right-nested: tuple on the right side of outer Prod --- */
    /* f expects (Bool, (Int, String)). Inside inner parens with one arg. */
    scaffold_test(
      ~name="Right-nested: f(true, (4|",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (4¦",
      ~expect=Some(", ?"),
    ),
    /* Right-nested: just the inner open paren */
    scaffold_test(
      ~name="Right-nested: f(true, (|",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (¦",
      ~expect=Some("?, "),
    ),
    /* Right-nested: inner tuple complete, no scaffold */
    scaffold_test(
      ~name="Right-nested: f(true, (4, | complete",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (4, ¦",
      ~expect=None,
    ),
    /* Right-nested: outer scaffold when only first arg typed */
    scaffold_test(
      ~name="Right-nested: f(true| outer scaffold",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true¦",
      ~expect=Some(", ?"),
    ),
    /* Right-nested with both parens: f(true, (4|)) */
    scaffold_test(
      ~name="Right-nested both parens: f(true, (4|))",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (4¦))",
      ~expect=Some(", ?"),
    ),
    /* Deeply nested: (Int, (String, (Bool, Float))) */
    scaffold_test(
      ~name="Deep right-nested: inner-most",
      ~code=
        "let f : (Int, (String, (Bool, Float))) -> Int = fun x -> 0 in f(1, (\"a\", (true¦",
      ~expect=Some(", ?"),
    ),
    /* Deep right-nested: middle level */
    scaffold_test(
      ~name="Deep right-nested: middle level",
      ~code=
        "let f : (Int, (String, (Bool, Float))) -> Int = fun x -> 0 in f(1, (\"a\"¦",
      ~expect=Some(", ?"),
    ),
    /* Middle-nested: (Int, (String, Bool), Float) */
    scaffold_test(
      ~name="Middle-nested: inside inner parens",
      ~code=
        "let f : (Int, (String, Bool), Float) -> Int = fun x -> 0 in f(1, (\"a\"¦",
      ~expect=Some(", ?"),
    ),
    /* Middle-nested: outer level after inner complete */
    scaffold_test(
      ~name="Middle-nested: outer after inner typed",
      ~code=
        "let f : (Int, (String, Bool), Float) -> Int = fun x -> 0 in f(1, (\"a\", true)¦)",
      ~expect=Some(", ?"),
    ),
    /* Right-nested Tab acceptance */
    accept_test(
      ~name="Right-nested: Tab inside inner parens",
      ~code=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (4¦",
      ~goal=
        "let f : (Bool, (Int, String)) -> Float = fun x -> 0.0 in f(true, (4, ¦?",
    ),
  ],
);

/* ---- Scaffold generation: labeled tuples (Phase 6) ---- */

let labeled_tests = (
  "TyDiScaffold.Labeled",
  [
    /* Labeled tuple: f expects (x=Int, y=String). After first arg,
     * should show scaffold with label: ", y=?" */
    scaffold_test(
      ~name="Labeled tuple: after first arg",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(1¦)",
      ~expect=Some(", y=?"),
    ),
    /* 3-element labeled: after first arg, shows labels for remaining */
    scaffold_test(
      ~name="Labeled 3-elem: after first arg",
      ~code="let f : (a=Int, b=String, c=Bool) -> Int = fun x -> 0 in f(1¦)",
      ~expect=Some(", b=?, c=?"),
    ),
    /* Unlabeled elements in a mixed tuple should show ? without label */
    scaffold_test(
      ~name="Mixed labeled/unlabeled",
      ~code="let f : (Int, y=String) -> Bool = fun a -> true in f(1¦)",
      ~expect=Some(", y=?"),
    ),
    /* Grout-right case: f( with labeled tuple should show first label */
    scaffold_test(
      ~name="Labeled grout-right: first label shown",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(¦",
      ~expect=Some("x=?, "),
    ),
  ],
);

/* ---- Scaffold generation: edge cases ---- */

let edge_tests = (
  "TyDiScaffold.Edge",
  [
    /* 4-arg function: full scaffold from empty */
    scaffold_test(
      ~name="4-arg: empty after open paren",
      ~code="let f : (Int, String, Bool, Float) -> Int = fun x -> 0 in f(¦",
      ~expect=Some("?, ?, ?, "),
    ),
    /* Function returning a function: scaffold for inner call */
    scaffold_test(
      ~name="Higher-order: scaffold for returned function",
      ~code=
        "let f : (Int, String) -> (Bool, Float) -> Int = fun x -> fun y -> 0 in f(1¦",
      ~expect=Some(", ?"),
    ),
    /* Caret on empty hole between commas: no scaffold (all commas present) */
    scaffold_test(
      ~name="Between existing commas: no scaffold",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦, true",
      ~expect=None,
    ),
    /* Shape fitting: ( is concave-right, 1 is convex-left → start with hole.
     * Caret between ( and existing content: scaffold must fit both sides. */
    scaffold_test(
      ~name="Shape fit: f(|1 → ?, (not , ?)",
      ~code="let f : (Bool, Int) -> Float = fun x -> 0.0 in f(¦1",
      ~expect=Some("?, "),
    ),
    /* 3-arg shape fit: g(|1 → ?, ?,  */
    scaffold_test(
      ~name="Shape fit: g(|1 → ?, ?, ",
      ~code="let g : (Bool, Int, String) -> Float = fun x -> 0.0 in g(¦1",
      ~expect=Some("?, ?, "),
    ),
    /* Trailing hole omitted: convex tile past grout on right.
     * f(1|~ 1 → just ", " (no trailing ? since 1 fills that position) */
    scaffold_test(
      ~name="Trailing hole: f(1| 1 → , (no hole)",
      ~code="let f : (Bool, Int) -> Float = fun x -> 0.0 in f(1¦ 1",
      ~expect=Some(", "),
    ),
    /* Trailing hole kept: only grout to right, no tile */
    scaffold_test(
      ~name="Trailing hole: f(1| → , ? (hole needed)",
      ~code="let f : (Bool, Int) -> Float = fun x -> 0.0 in f(1¦",
      ~expect=Some(", ?"),
    ),
    /* 3-arg trailing hole omitted: g(1| 2 → , ?, (interior hole kept) */
    scaffold_test(
      ~name="Trailing hole: g(1| 2 → , ?, (interior kept)",
      ~code="let g : (Bool, Int, String) -> Float = fun x -> 0.0 in g(1¦ 2",
      ~expect=Some(", ?, "),
    ),
    /* holes_first with tile on right: f(| 1 → ?,  */
    scaffold_test(
      ~name="Holes first + trailing: f(| 1 → ?, ",
      ~code="let f : (Bool, Int) -> Float = fun x -> 0.0 in f(¦ 1",
      ~expect=Some("?, "),
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
 *       ~expect=Some("?, "),
 *     ),
 *   ],
 * );
 */

/* ---- Tab acceptance tests ---- */

let acceptance_tests = (
  "TyDiScaffold.Acceptance",
  [
    /* Shard case: f(1▎ → Tab → f(1, ¦? (with space after comma) */
    accept_test(
      ~name="Shard: 2-arg Tab inserts comma+space",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in f(1, ¦?",
    ),
    /* Shard case: g(1▎ → Tab → g(1, ¦? */
    accept_test(
      ~name="Shard: 3-arg Tab inserts one comma+space",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦",
      ~goal="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦?",
    ),
    /* Ancestor case: f(1▎) → Tab → f(1, ¦?) */
    accept_test(
      ~name="Ancestor: 2-arg Tab inserts comma+space",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦)",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in f(1, ¦?)",
    ),
    /* Explicit parens: (1▎) → Tab → (1, ¦?) */
    accept_test(
      ~name="Explicit parens: Tab inserts comma+space",
      ~code="let t : (Int, Bool) = (1¦) in t",
      ~goal="let t : (Int, Bool) = (1, ¦?) in t",
    ),
    /* Labeled grout-right: f(▎ → Tab → f(x=▎ — emits label prefix */
    accept_test(
      ~name="Labeled grout-right: Tab inserts label",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(¦",
      ~goal="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=¦?",
    ),
    /* Labeled after value: f(1▎ → Tab → f(1, y=▎ — comma + label */
    accept_test(
      ~name="Labeled after value: Tab inserts comma+label",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(1¦)",
      ~goal="let f : (x=Int, y=String) -> Bool = fun a -> true in f(1, ¦?)",
    ),
  ],
);

let def2 = "let f : (Int, String) -> Int = fun x -> 0 in ";
let def3 = "let g : (Int, String, Bool) -> Int = fun x -> 0 in ";

/* ---- Progressive character-by-character tests ---- */

let progressive_tests = (
  "TyDiScaffold.Progressive",
  [
    /* f( → scaffold for 2-arg */
    scaffold_test(
      ~name="f(: shows ?, ",
      ~code=def2 ++ "f(¦",
      ~expect=Some("?, "),
    ),
    /* f(1 → scaffold should still show */
    scaffold_test(
      ~name="f(1: shows , ?",
      ~code=def2 ++ "f(1¦",
      ~expect=Some(", ?"),
    ),
    /* f(1, → all commas present, no scaffold */
    scaffold_test(
      ~name="f(1,: no scaffold",
      ~code=def2 ++ "f(1, ¦",
      ~expect=None,
    ),
    /* 3-arg progressive: g( */
    scaffold_test(
      ~name="g(: shows ?, ?, ",
      ~code=def3 ++ "g(¦",
      ~expect=Some("?, ?, "),
    ),
    /* g(1 */
    scaffold_test(
      ~name="g(1: shows , ?, ?",
      ~code=def3 ++ "g(1¦",
      ~expect=Some(", ?, ?"),
    ),
    /* g(1, */
    scaffold_test(
      ~name="g(1,: shows ?, ",
      ~code=def3 ++ "g(1, ¦",
      ~expect=Some("?, "),
    ),
    /* g(1, t */
    scaffold_test(
      ~name="g(1, t: shows , ?",
      ~code=def3 ++ "g(1, t¦",
      ~expect=Some(", ?"),
    ),
    /* g(1, true */
    scaffold_test(
      ~name="g(1, true: shows , ? (suppressed if type matches?)",
      ~code=def3 ++ "g(1, true¦",
      ~expect=Some(", ?"),
    ),
    /* g(1, true, → all commas present */
    scaffold_test(
      ~name="g(1, true,: no scaffold",
      ~code=def3 ++ "g(1, true, ¦",
      ~expect=None,
    ),
    /* Nested: f(( → after opening inner paren.
     * Nested ( shards: inner ( finds outer (, which finds f.
     * Peels first Prod element: (Int, Int) from ((Int, Int), String). */
    scaffold_test(
      ~name="Nested f((: inner scaffold",
      ~code="let f : ((Int, Int), String) -> Bool = fun x -> true in f((¦",
      ~expect=Some("?, "),
    ),
    /* Nested: f((1 → inside inner parens with content. */
    scaffold_test(
      ~name="Nested f((1: inner scaffold",
      ~code="let f : ((Int, Int), String) -> Bool = fun x -> true in f((1¦",
      ~expect=Some(", ?"),
    ),
    /* Nested: f((1, → inner comma placed, no more needed */
    scaffold_test(
      ~name="Nested f((1,: no scaffold",
      ~code="let f : ((Int, Int), String) -> Bool = fun x -> true in f((1, ¦",
      ~expect=None,
    ),
    /* Nested: f((1, 2 → inner tuple complete, no scaffold */
    scaffold_test(
      ~name="Nested f((1, 2: no scaffold",
      ~code=
        "let f : ((Int, Int), String) -> Bool = fun x -> true in f((1, 2¦",
      ~expect=None,
    ),
  ],
);

let pattern_tests = (
  "TyDiScaffold.Pattern",
  [
    /* Pattern: fun (¦ — open paren shard in pattern, tuple expected */
    scaffold_test(
      ~name="Pattern fun (: scaffold",
      ~code="let f : (Int, String) -> Bool = fun (¦",
      ~expect=Some("?, "),
    ),
    /* Pattern: fun (x¦ — content in pattern paren */
    scaffold_test(
      ~name="Pattern fun (x: scaffold",
      ~code="let f : (Int, String) -> Bool = fun (x¦",
      ~expect=Some(", ?"),
    ),
    /* Pattern: fun (x,¦ — comma placed, no more needed */
    scaffold_test(
      ~name="Pattern fun (x,: no scaffold",
      ~code="let f : (Int, String) -> Bool = fun (x, ¦",
      ~expect=None,
    ),
    /* Pattern: 3-element function arg */
    scaffold_test(
      ~name="Pattern fun ( 3-arg: scaffold",
      ~code="let g : (Int, String, Bool) -> Bool = fun (x¦",
      ~expect=Some(", ?, ?"),
    ),
  ],
);

/* ---- Multi-Tab acceptance: sequential Tab presses ---- */

let multi_tab_tests = (
  "TyDiScaffold.MultiTab",
  [
    /* 3-arg: Tab-Tab from g(1¦ → g(1, ?, ¦? */
    multi_accept_test(
      ~name="3-arg: two Tabs from g(1",
      ~code=def3 ++ "g(1¦",
      ~n_tabs=2,
      ~goal=def3 ++ "g(1, ?, ¦?",
    ),
    /* 3-arg: Tab-Tab from g(¦ → g(?, ?, ¦?
     * First Tab emits hole+comma (scaffold holes_first chunk),
     * second Tab emits another comma. */
    multi_accept_test(
      ~name="3-arg: two Tabs from g(",
      ~code=def3 ++ "g(¦",
      ~n_tabs=2,
      ~goal=def3 ++ "g(?, ?, ¦?",
    ),
    /* 2-arg: single Tab from f(1¦ → f(1, ¦? */
    multi_accept_test(
      ~name="2-arg: one Tab from f(1",
      ~code=def2 ++ "f(1¦",
      ~n_tabs=1,
      ~goal=def2 ++ "f(1, ¦?",
    ),
    /* Labeled 2-arg: Tab from f(¦ → f(x=¦?
     * Inserts label prefix "x=" */
    multi_accept_test(
      ~name="Labeled: one Tab from f(",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(¦",
      ~n_tabs=1,
      ~goal="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=¦?",
    ),
  ],
);

/* ---- Labeled acceptance: progressive Tab ---- */

let labeled_accept_tests = (
  "TyDiScaffold.LabeledAcceptance",
  [
    /* f( with labeled → Tab inserts x= (label prefix) */
    accept_test(
      ~name="Labeled: Tab on f( inserts label",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(¦",
      ~goal="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=¦?",
    ),
    /* After user types value: f(x=1▎) → scaffold ", y=?" → Tab inserts , */
    accept_test(
      ~name="Labeled: Tab after value inserts comma",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=1¦)",
      ~goal="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=1, ¦?)",
    ),
    /* 3-labeled: f( → Tab → f(a= */
    accept_test(
      ~name="Labeled 3-elem: Tab on f( inserts first label",
      ~code="let f : (a=Int, b=String, c=Bool) -> Int = fun x -> 0 in f(¦",
      ~goal="let f : (a=Int, b=String, c=Bool) -> Int = fun x -> 0 in f(a=¦?",
    ),
    /* After accepting label: f(x=¦ should show ", y=?" not "x=?, "
     * (no label duplication — grout_right is false with content to left) */
    scaffold_test(
      ~name="Labeled after label accept: no duplication",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=¦",
      ~expect=Some(", y=?"),
    ),
  ],
);

/* ---- Scaffold after comma (Issue 2 behavior) ---- */

let after_comma_tests = (
  "TyDiScaffold.AfterComma",
  [
    /* After typing comma in 3-arg: g(1, ¦ → scaffold "?, " because
     * there's grout to right and 1 remaining comma */
    scaffold_test(
      ~name="After comma 3-arg: g(1, shows scaffold",
      ~code=def3 ++ "g(1, ¦",
      ~expect=Some("?, "),
    ),
    /* After typing all commas: g(1, true, ¦ → no scaffold */
    scaffold_test(
      ~name="After all commas: no scaffold",
      ~code=def3 ++ "g(1, true, ¦",
      ~expect=None,
    ),
    /* After comma in 2-arg: f(1, ¦ → no scaffold (all commas placed) */
    scaffold_test(
      ~name="After comma 2-arg: no scaffold",
      ~code=def2 ++ "f(1, ¦",
      ~expect=None,
    ),
    /* Labeled: after typing comma, scaffold for next position.
     * f(x=1, ¦ → no scaffold (all commas present in 2-elem tuple) */
    scaffold_test(
      ~name="Labeled after comma: no scaffold",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(x=1, ¦",
      ~expect=None,
    ),
    /* After typing comma without space: g(1,¦ → scaffold should have
     * leading space for formatting: " ?, " not "?, " */
    scaffold_test(
      ~name="After comma no space: leading space in scaffold",
      ~code=def3 ++ "g(1,¦",
      ~expect=Some(" ?, "),
    ),
    /* 2-arg after comma no space: f(1,¦ → no remaining commas but
     * still no scaffold (all commas placed) */
    scaffold_test(
      ~name="After comma no space 2-arg: no scaffold",
      ~code=def2 ++ "f(1,¦",
      ~expect=None,
    ),
    /* After comma no space: Tab inserts just the formatting space.
     * g(1,¦ → Tab → g(1, ¦? — caret ready for next arg */
    accept_test(
      ~name="After comma no space: Tab inserts space",
      ~code=def3 ++ "g(1,¦",
      ~goal=def3 ++ "g(1, ¦?",
    ),
  ],
);

/* ---- Incomplete forms: scaffold inside unclosed let/fun/if ---- */

/* These test scaffold behavior when the caret is inside incomplete
 * syntax — e.g., a let without its `in`, or a function body without
 * closing delimiters. This is the common case when writing new code
 * above existing lines. The Dump step completes these forms before
 * statics, so scaffold should still work. */

let incomplete_tests = (
  "TyDiScaffold.Incomplete",
  [
    /* let without in: let x = f(1¦ — incomplete let, open paren shard */
    scaffold_test(
      ~name="let-no-in: shard case",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let x = f(1¦",
      ~expect=Some(", ?"),
    ),
    /* let without in: ancestor case with both parens */
    scaffold_test(
      ~name="let-no-in: ancestor case",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let x = f(1¦)",
      ~expect=Some(", ?"),
    ),
    /* fun body: fun x -> f(1¦ — incomplete fun, open paren */
    scaffold_test(
      ~name="fun-body: shard case",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in fun x -> f(1¦",
      ~expect=Some(", ?"),
    ),
    /* if-then: if true then f(1¦ — incomplete if, no else */
    scaffold_test(
      ~name="if-then: shard case",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in if true then f(1¦",
      ~expect=Some(", ?"),
    ),
    /* Nested incomplete let: let y = 0 in let x = f(1¦ */
    scaffold_test(
      ~name="nested-let: shard case",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let y = 0 in let x = f(1¦",
      ~expect=Some(", ?"),
    ),
    /* Writing above existing code: let x = f(1¦ followed by more code.
     * The trailing let simulates code on the next line. */
    scaffold_test(
      ~name="above-existing-code: shard",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let x = f(1¦\nlet y = 2 in y",
      ~expect=Some(", ?"),
    ),
    /* Grout-right in incomplete let */
    scaffold_test(
      ~name="let-no-in: grout-right",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let x = f(¦",
      ~expect=Some("?, "),
    ),
    /* 3-arg incomplete let */
    scaffold_test(
      ~name="let-no-in: 3-arg",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in let x = g(1¦",
      ~expect=Some(", ?, ?"),
    ),
    /* Labeled in incomplete let */
    scaffold_test(
      ~name="let-no-in: labeled",
      ~code=
        "let f : (x=Int, y=String) -> Bool = fun a -> true in let r = f(1¦",
      ~expect=Some(", y=?"),
    ),
    /* Tab acceptance in incomplete form.
     * Note: printer doesn't show Dump-completed `in` — it only renders
     * the zipper's actual content, not backpack shards. */
    accept_test(
      ~name="let-no-in: Tab acceptance",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let x = f(1¦",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in let x = f(1, ¦?",
    ),
  ],
);

/* ---- Pattern ancestor case: both parens placed in pattern ---- */

let pattern_ancestor_tests = (
  "TyDiScaffold.PatternAncestor",
  [
    /* let (¦) : (Int, Bool) = ... — caret is Inner(0) on the ( shard.
     *
     * The ( tile is the *right sibling* of the caret, not an ancestor or
     * left sibling. So inside_parens returns false. Moreover, the siblings
     * seen by scaffold (count_commas, should_suppress, grout_right) are the
     * pieces OUTSIDE the parens in the let body — wrong context entirely.
     *
     * Fixing this requires either:
     * (a) Virtual move: copy zipper, move caret to Outer inside paren child,
     *     run scaffold on the copy.
     * (b) Direct child access: extract the tile's child segment and analyze
     *     its content (commas, grout, pieces) directly.
     *
     * Both approaches are non-trivial. Skipped for now. */
    test_case(
      "Pattern ancestor: let (|) caret=Inner",
      `Quick,
      () => {
        let code = "let (¦) : (Int, Bool) = (1, true) in 0";
        let result = assist_suggest(code);
        /* Currently returns None -- documenting actual behavior */
        check(option(string), "pattern ancestor returns None", None, result);
      },
    ),
  ],
);

/* ---- Multi-line / mid-program: scaffold in realistic editing contexts ----
 *
 * These test scaffold behavior when the caret is in the middle of an
 * existing program — the common case when writing a new call between
 * existing definitions. The trailing code exercises incomplete-form
 * handling and ensures scaffold works with surrounding syntax. */

let multiline_tests = (
  "TyDiScaffold.Multiline",
  [
    /* Writing a call between two let bindings */
    scaffold_test(
      ~name="Mid-program: call between lets",
      ~code=
        {|let f : (Int, String) -> Int = fun x -> 0 in
let result = f(1¦
let other = 5 in
other|},
      ~expect=Some(", ?"),
    ),
    /* Empty call between definitions */
    scaffold_test(
      ~name="Mid-program: empty call between lets",
      ~code=
        {|let f : (Int, String) -> Int = fun x -> 0 in
let result = f(¦
let other = 5 in
other|},
      ~expect=Some("?, "),
    ),
    /* 3-arg call in the middle of a program */
    scaffold_test(
      ~name="Mid-program: 3-arg call",
      ~code=
        {|let g : (Int, String, Bool) -> Int = fun x -> 0 in
let r = g(1¦
let y = "hello" in
y|},
      ~expect=Some(", ?, ?"),
    ),
    /* Suppression mid-program: tuple var satisfies Prod */
    scaffold_test(
      ~name="Mid-program: suppression with tuple var",
      ~code=
        {|let f : (Int, String) -> Int = fun x -> 0 in
let p : (Int, String) = (1, "a") in
let r = f(p¦
let other = 5 in
other|},
      ~expect=None,
    ),
    /* Suppression mid-program: blargs satisfies full Prod */
    scaffold_test(
      ~name="Mid-program: blargs suppression",
      ~code=
        {|let blargs : (String, String, String) = ? in
"" ++ string_replace(blargs¦
let x = 1 in
x|},
      ~expect=None,
    ),
    /* Element completion mid-program: caret right after ar on same line.
     * Note: multi-line trailing code (with \n) causes caret positioning
     * issues in the test harness — the move-left-by-N-chars approach
     * doesn't always land precisely after a linebreak. Using single-line
     * incomplete form instead (no \n). */
    scaffold_test(
      ~name="Mid-program: element completion (incomplete let)",
      ~code="let arg : String = ? in let r = string_replace(ar¦",
      ~expect=Some("g, ?, ?"),
    ),
    /* Multi-line function application: arguments on separate lines.
     * Caret after 1 on an indented line, ( is on previous line. */
    scaffold_test(
      ~name="Multi-line app: f(\\n  1|",
      ~code={|let f : (Int, String) -> Int = fun x -> 0 in
f(
  1¦|},
      ~expect=Some(", ?"),
    ),
    /* Multi-line: caret on empty indented line after ( */
    scaffold_test(
      ~name="Multi-line app: f(\\n  |",
      ~code={|let f : (Int, String) -> Int = fun x -> 0 in
f(
  ¦|},
      ~expect=Some("?, "),
    ),
    /* Multi-line: caret after partial variable, f not in scope.
     * No scaffold (f has Unknown type), but text completion still
     * finds arg as a prefix match. */
    scaffold_test(
      ~name="Multi-line app: f(\\n  ar| (no scaffold, text completion)",
      ~code={|let arg : String = ? in
f(
  ar¦|},
      ~expect=Some("g"),
    ),
    /* Multi-line with function in scope */
    scaffold_test(
      ~name="Multi-line app: string_replace(\\n  ar|",
      ~code={|let arg : String = ? in
string_replace(
  ar¦|},
      ~expect=Some("g, ?, ?"),
    ),
  ],
);

/* ---- Segment well-formedness: buffer splice must not crash Skel ---- */

/* After set_scaffold, the buffer content gets spliced into the segment
 * via unselect_and_zip. If the resulting segment has shape conflicts
 * (e.g. concave-concave adjacency), MakeTerm.go → Segment.skel → Skel.mk
 * will crash with "split_kids: index out of bounds".
 *
 * These tests verify that the segment produced by unselect_and_zip is
 * well-formed for every scaffold scenario — including deletion cases
 * where concave grout may be adjacent to buffer comma tiles. */

/* Build zipper, set scaffold, then verify unselect_and_zip produces
 * a segment that MakeTerm.go can process without crashing. */
let scaffold_segment_ok = (code: string): bool => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let z = TyDiScaffold.set(~info_map, z);
  /* This is what CachedSyntax.mk does — if it crashes, the UI crashes */
  let segment = Zipper.unselect_and_zip(z);
  switch (MakeTerm.go(segment)) {
  | _ => true
  | exception (Failure(_)) => false
  };
};

let segment_ok_test = (~name, ~code) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.bool, Bool.equal),
      name ++ ": segment must be Skel-safe",
      true,
      scaffold_segment_ok(code),
    )
  );

let segment_wellformedness_tests = (
  "TyDiScaffold.SegmentOk",
  [
    /* Basic shard cases: buffer spliced into right siblings */
    segment_ok_test(
      ~name="Shard: f(1| buffer=', ?'",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
    ),
    segment_ok_test(
      ~name="Shard: f(| buffer='?, '",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(¦",
    ),
    segment_ok_test(
      ~name="Shard 3-arg: g(1| buffer=', ?, '",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1¦",
    ),
    /* Ancestor cases: both parens placed */
    segment_ok_test(
      ~name="Ancestor: f(1|) buffer=', ?'",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦)",
    ),
    segment_ok_test(
      ~name="Ancestor: g(1, 2|) buffer=', ?'",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, 2¦)",
    ),
    /* Edge case: caret between two args with concave grout.
     * After deleting a comma, regrout inserts concave grout.
     * The buffer's comma tile creates concave-concave conflict. */
    segment_ok_test(
      ~name="Between args: f(1| 2) concave grout conflict",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦ 2)",
    ),
    segment_ok_test(
      ~name="Between args: f(1 |2) concave grout conflict",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1 ¦2)",
    ),
    /* Incomplete forms */
    segment_ok_test(
      ~name="Incomplete let: let x = f(1|",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in let x = f(1¦",
    ),
    segment_ok_test(
      ~name="Incomplete fun: fun x -> f(1|",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in fun x -> f(1¦",
    ),
    /* Labeled tuples */
    segment_ok_test(
      ~name="Labeled: f(1| y=?)",
      ~code="let f : (x=Int, y=String) -> Bool = fun a -> true in f(1¦)",
    ),
    /* Holes-first patterns (grout-right): left edge is convex (hole),
     * right edge is concave (comma). Need to strip right-side grout. */
    segment_ok_test(
      ~name="Grout-right: f(| grout",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(¦",
    ),
    segment_ok_test(
      ~name="Grout-right 3-arg: g(| grout",
      ~code="let g : (Int, String, Bool) -> Int = fun x -> 0 in g(¦",
    ),
    /* Trailing code after parens */
    segment_ok_test(
      ~name="Trailing: f(1|) + 2",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦) + 2",
    ),
    /* Nested calls */
    segment_ok_test(
      ~name="Nested: h(f(1|))",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let h : Int -> Int = fun y -> y in h(f(1¦))",
    ),
  ],
);

/* ---- Suppression: scaffold should not appear when value satisfies Prod ----
 *
 * When the expression at the caret already has a type consistent with the
 * full expected Prod type, scaffold is suppressed — no commas/holes needed.
 * This covers:
 * - Fully typed variable of tuple type (e.g., blargs : (String, String, String))
 * - Ancestor case (both parens placed) and shard case () in backpack)
 * - Single-typed values should NOT suppress (e.g., arg : String in 3-arg call)
 *
 * Suppression uses synthesized type (Self.typ_of_exp) to avoid stale
 * reconciled types from preserve_grout_id. */

let suppression_tests = (
  "TyDiScaffold.Suppression",
  [
    /* Ancestor case: f(p▎) where p : (Int, String) — parens complete */
    scaffold_test(
      ~name="Ancestor: tuple var suppresses scaffold",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let p : (Int, String) = (1, \"a\") in f(p¦)",
      ~expect=None,
    ),
    /* Shard case: f(p▎ with ) in backpack */
    scaffold_test(
      ~name="Shard: tuple var suppresses scaffold",
      ~code=
        "let f : (Int, String) -> Int = fun x -> 0 in let p : (Int, String) = (1, \"a\") in f(p¦) + 1",
      ~expect=None,
    ),
    /* 3-arg builtin: string_replace(blargs▎ */
    scaffold_test(
      ~name="3-arg: tuple var suppresses scaffold",
      ~code=
        "let blargs : (String, String, String) = ? in string_replace(blargs¦",
      ~expect=None,
    ),
    /* No suppression: single-typed var doesn't satisfy Prod */
    scaffold_test(
      ~name="No suppress: single-typed var in 3-arg call",
      ~code="let arg : String = ? in string_replace(arg¦",
      ~expect=Some(", ?, ?"),
    ),
    /* No suppression: empty hole (no value to check) */
    scaffold_test(
      ~name="No suppress: empty hole after open paren",
      ~code=def2 ++ "f(¦",
      ~expect=Some("?, "),
    ),
    /* No suppression: value with wrong tuple arity */
    scaffold_test(
      ~name="No suppress: wrong-arity tuple var",
      ~code="let pair : (String, String) = ? in string_replace(pair¦",
      ~expect=Some(", ?, ?"),
    ),
  ],
);

/* ---- Completion + suppression interaction ----
 *
 * When TyDi suggests completing a variable whose type would satisfy the
 * full expected Prod, the scaffold should be omitted from the combined
 * buffer. The completion alone is the right suggestion.
 *
 * set_assist_buffer combines completion and scaffold. When both apply,
 * it should check whether the completed variable's type is consistent
 * with the expected Prod and, if so, drop the scaffold.
 *
 * These tests use set_assist_buffer (via Buffer.set_assist_buffer) to
 * verify the combined behavior. */

let completion_suppression_tests = (
  "TyDiScaffold.CompletionSuppression",
  [
    /* Typing bl▎ inside string_replace( where blargs : (String,String,String)
     * is in scope. TyDi suggests "args" completion. Since completing would
     * produce a value satisfying the full Prod, scaffold should be omitted.
     * Buffer should show just "args" (completion only, no scaffold). */
    scaffold_test(
      ~name="Completion satisfies Prod: no scaffold",
      ~code="let blargs : (String, String, String) = ? in string_replace(bl¦",
      ~expect=Some("args"),
    ),
    /* Typing ar▎ where arg : String AND args : (String,String,String).
     * Element-type completion (arg, shorter) preferred over full-Prod
     * completion (args). Result: "g" + scaffold for remaining elements. */
    scaffold_test(
      ~name="Element completion + scaffold preferred over full Prod",
      ~code=
        "let args : (String, String, String) = ? in let arg : String = ? in string_replace(ar¦",
      ~expect=Some("g, ?, ?"),
    ),
    /* Typing ar▎ where only arg : String (no Prod match).
     * Element completion finds arg, combined with scaffold. */
    scaffold_test(
      ~name="Element completion only: arg + scaffold",
      ~code="let arg : String = ? in string_replace(ar¦",
      ~expect=Some("g, ?, ?"),
    ),
    /* arg fully typed: element-type exact match suppresses completion.
     * Should show scaffold only, not suggest "args" via full-Prod path. */
    scaffold_test(
      ~name="Exact element match: scaffold only, no completion",
      ~code=
        "let args : (String, String, String) = ? in let arg : String = ? in string_replace(arg¦",
      ~expect=Some(", ?, ?"),
    ),
    /* Form completion in 3-arg context: g(1111, tr → "ue, ?"
     * TyDi finds "true" via form suggestions. Since true : Bool
     * doesn't satisfy the full Prod (Int, String, Bool), scaffold
     * for the remaining 3rd arg is combined with the completion. */
    scaffold_test(
      ~name="Form completion + scaffold: g(1111, tr",
      ~code=def3 ++ "g(1111, tr¦",
      ~expect=Some("ue, ?"),
    ),
    /* No completion available, just scaffold */
    scaffold_test(
      ~name="No completion, just scaffold",
      ~code=def2 ++ "f(1¦",
      ~expect=Some(", ?"),
    ),
  ],
);

/* ---- Integration: init_with_assist produces correct buffer + statics ----
 *
 * These test CachedStatics.init_with_assist, the single function that
 * resolves the buffer↔statics circular dependency:
 *
 * 1. Run statics on bare zipper (no buffer) → info_map
 * 2. Compute assist buffer (completion + scaffold) using that info_map
 * 3. If scaffold generated, re-run statics so elaboration sees the tuple
 *
 * Returns (buffer, statics) — both correct by construction. */

let init_with_assist_result = (code: string): (option(string), bool) => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  let (z, statics) =
    CachedStatics.init_with_assist(
      ~settings=CoreSettings.on,
      ~is_dynamic_term=false,
      ~stitch=x => x,
      z,
    );
  let buffer = TyDi.get_unparsed_buffer(z);
  let has_errors = statics.error_ids != [];
  (buffer, has_errors);
};

let integration_test = (~name, ~code, ~expect_buffer, ~expect_errors) =>
  test_case(
    name,
    `Quick,
    () => {
      let (buffer, has_errors) = init_with_assist_result(code);
      check(option(string), name ++ " buffer", expect_buffer, buffer);
      check(
        testable(Fmt.bool, Bool.equal),
        name ++ " errors",
        expect_errors,
        has_errors,
      );
    },
  );

let integration_tests = (
  "TyDiScaffold.Integration",
  [
    /* string_replace(¦ — scaffold should appear, no type errors
     * (scaffold is reified so statics sees the full tuple) */
    integration_test(
      ~name="After open paren: scaffold, no errors",
      ~code="let blargs : (String, String, String) = ? in string_replace(¦",
      ~expect_buffer=Some("?, ?, "),
      ~expect_errors=false,
    ),
    /* string_replace(blargs¦ — no scaffold (suppressed), no type errors */
    integration_test(
      ~name="Tuple var typed: no scaffold, no errors",
      ~code=
        "let blargs : (String, String, String) = ? in string_replace(blargs¦",
      ~expect_buffer=None,
      ~expect_errors=false,
    ),
    /* string_replace(bl¦ — completion "args", no scaffold.
     * Errors expected: "bl" is not a complete identifier, so statics
     * flags it. The completion is ghost text, not yet accepted. */
    integration_test(
      ~name="Partial var: completion only, errors on incomplete id",
      ~code="let blargs : (String, String, String) = ? in string_replace(bl¦",
      ~expect_buffer=Some("args"),
      ~expect_errors=true,
    ),
    /* string_replace(arg¦ where arg : String — scaffold, no errors
     * (scaffold reification gives arg the correct per-element ana) */
    integration_test(
      ~name="Single-typed var with scaffold: no errors",
      ~code="let arg : String = ? in string_replace(arg¦",
      ~expect_buffer=Some(", ?, ?"),
      ~expect_errors=false,
    ),
    /* string_replace(1¦ — scaffold, has errors (Int vs String) */
    integration_test(
      ~name="Wrong type literal: scaffold, has errors",
      ~code="let blargs : (String, String, String) = ? in string_replace(1¦",
      ~expect_buffer=Some(", ?, ?"),
      ~expect_errors=true,
    ),
  ],
);

let tests = [
  shard_tests,
  ancestor_tests,
  midexpr_tests,
  nested_tests,
  labeled_tests,
  edge_tests,
  acceptance_tests,
  multi_tab_tests,
  labeled_accept_tests,
  after_comma_tests,
  progressive_tests,
  pattern_tests,
  incomplete_tests,
  pattern_ancestor_tests,
  multiline_tests,
  suppression_tests,
  completion_suppression_tests,
  integration_tests,
  segment_wellformedness_tests,
];
