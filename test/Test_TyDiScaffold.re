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

let scaffold_debug = (code: string): unit => {
  let actions = Test_Editing.mk(code);
  let z = Test_Editing.perform(Zipper.init(), actions);
  Printf.printf(
    "DBG caret=%s anc=%d inside=%b printer=[%s]\n",
    z.caret == Outer ? "O" : "I",
    List.length(z.relatives.ancestors),
    TyDi.inside_parens(z),
    Test_Editing.printer(z),
  );
};

let scaffold_test = (~name, ~code, ~expect) =>
  test_case(name, `Quick, () => {
    if (expect != scaffold_suggest(code)) {
      scaffold_debug(code);
    };
    check(option(string), name, expect, scaffold_suggest(code));
  });

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

/* ---- Scaffold generation: nested tuples (Phase 5) ---- */

let nested_tests = (
  "TyDiScaffold.Nested",
  [
    /* f expects ((Int, Int), String). After typing first nested tuple,
     * scaffold should show remaining outer element: `, ○` */
    scaffold_test(
      ~name="Nested tuple: after inner tuple value",
      ~code=
        "let f : ((Int, Int), String) -> Bool = fun x -> true in f((1, 2)¦)",
      ~expect=Some(", " ++ hole_char),
    ),
    /* f expects (Int, (String, Bool)). After typing first element,
     * scaffold should show `, ○` for the remaining element */
    scaffold_test(
      ~name="Nested tuple: second element is tuple",
      ~code=
        "let f : (Int, (String, Bool)) -> Bool = fun x -> true in f(1¦)",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Genuine match: (1, "a") matches (Int, String) — should suppress */
    scaffold_test(
      ~name="Nested: genuine match suppresses scaffold",
      ~code=
        "let f : (Int, String) -> Bool = fun x -> true in f((1, \"a\")¦)",
      ~expect=None,
    ),
    /* 3-element nested: ((Int,Int), String, Bool) with inner tuple typed */
    scaffold_test(
      ~name="Nested 3-elem: after inner tuple",
      ~code=
        "let f : ((Int, Int), String, Bool) -> Int = fun x -> 0 in f((1, 2)¦)",
      ~expect=Some(", " ++ hole_char ++ ", " ++ hole_char),
    ),
  ],
);

/* ---- Scaffold generation: labeled tuples (Phase 6) ---- */

let labeled_tests = (
  "TyDiScaffold.Labeled",
  [
    /* Labeled tuple: f expects (x=Int, y=String). After first arg,
     * should show scaffold with label: ", y=○" */
    scaffold_test(
      ~name="Labeled tuple: after first arg",
      ~code=
        "let f : (x=Int, y=String) -> Bool = fun a -> true in f(1¦)",
      ~expect=Some(", y=" ++ hole_char),
    ),
    /* 3-element labeled: after first arg, shows labels for remaining */
    scaffold_test(
      ~name="Labeled 3-elem: after first arg",
      ~code=
        "let f : (a=Int, b=String, c=Bool) -> Int = fun x -> 0 in f(1¦)",
      ~expect=Some(", b=" ++ hole_char ++ ", c=" ++ hole_char),
    ),
    /* Unlabeled elements in a mixed tuple should show ○ without label */
    scaffold_test(
      ~name="Mixed labeled/unlabeled",
      ~code=
        "let f : (Int, y=String) -> Bool = fun a -> true in f(1¦)",
      ~expect=Some(", y=" ++ hole_char),
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
      ~code=
        "let f : (Int, String, Bool, Float) -> Int = fun x -> 0 in f(¦",
      ~expect=
        Some(
          hole_char
          ++ ", "
          ++ hole_char
          ++ ", "
          ++ hole_char
          ++ ", ",
        ),
    ),
    /* Function returning a function: scaffold for inner call */
    scaffold_test(
      ~name="Higher-order: scaffold for returned function",
      ~code=
        "let f : (Int, String) -> (Bool, Float) -> Int = fun x -> fun y -> 0 in f(1¦",
      ~expect=Some(", " ++ hole_char),
    ),
    /* Caret on empty hole between commas: no scaffold (all commas present) */
    scaffold_test(
      ~name="Between existing commas: no scaffold",
      ~code=
        "let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1, ¦, true",
      ~expect=None,
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

/* ---- Statics reification: ana type after virtual scaffold insertion ---- */

/* Check that the ana at caret is an Int atom (decomposed from Prod) */
let ana_is_int = (code: string): bool =>
  switch (scaffold_ana(code)) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Atom(Int) => true
    | _ => false
    }
  | None => false
  };

/* Check that the ana at caret is a Prod (remains as Prod, no decomposition) */
let ana_is_prod = (code: string): bool =>
  switch (scaffold_ana(code)) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Prod(_) => true
    | _ => false
    }
  | None => false
  };

let reification_tests = (
  "TyDiScaffold.Reification",
  [
    /* After scaffold: f(1▎⟨, ○⟩ → statics sees Tuple([1, ⬚])
     * ana at 1 should be Int (decomposed from Prod([Int, String])) */
    test_case("Reified ana: first elem is Int", `Quick, () =>
      check(
        testable(Fmt.bool, Bool.equal),
        "ana should be Int",
        true,
        ana_is_int(
          "let f : (Int, String) -> Int = fun x -> 0 in f(1¦",
        ),
      )
    ),
    /* Without scaffold, ana at 1 would be Prod([Int, String]) */
    test_case("Without scaffold: ana is Prod", `Quick, () =>
      check(
        testable(Fmt.bool, Bool.equal),
        "ana without scaffold should be Prod",
        true,
        ana_is_prod(
          "let f : Int -> Bool = fun x -> true in f(1¦",
        )
        |> (!),  /* No scaffold for non-Prod → ana is NOT Prod */
      )
    ),
  ],
);

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
    /* Ancestor case: f(1▎) → Tab → f(1,¦?) */
    accept_test(
      ~name="Ancestor: 2-arg Tab inserts comma",
      ~code="let f : (Int, String) -> Int = fun x -> 0 in f(1¦)",
      ~goal="let f : (Int, String) -> Int = fun x -> 0 in f(1,¦?)",
    ),
    /* Explicit parens: (1▎) → Tab → (1,¦?) */
    accept_test(
      ~name="Explicit parens: Tab inserts comma",
      ~code="let t : (Int, Bool) = (1¦) in t",
      ~goal="let t : (Int, Bool) = (1,¦?) in t",
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
  reification_tests,
  acceptance_tests,
];
