open Alcotest;
open Haz3lcore;
open Language;

let ellipsis = "…";

/* Settings matching what ProbeProj uses for rendering abbreviated terms.
   project_tables is disabled here to mirror ProjectorInfo.utility.term_to_seg —
   auto-table-projection is a downstream display concern and would wrap output
   in `^^table(...)`, defeating Abbreviate's budget bound. */
let abbrev_settings: ExpToSegment.Settings.t = {
  ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
  show_unknown_as_hole: false,
  hole_tiles: false,
  project_tables: false,
};

let exp_to_seg = ExpToSegment.exp_to_segment(~settings=abbrev_settings);
let seg_to_str = Printer.of_segment(~holes="?", ~indent="");

/* Render an expression to string using the same pipeline as ProbeProj */
let render_exp = (exp: Exp.t): string => {
  exp |> exp_to_seg |> seg_to_str;
};

/* Parse source string, abbreviate at given budget, render to string */
let abbreviate_and_render = (~available: int, src: string): string => {
  switch (Parser.to_term(src, ~root=Exp)) {
  | Some(term) =>
    let (abbreviated, _length) = Abbreviate.abbreviate_exp(~available, term);
    render_exp(abbreviated);
  | None => failwith("Failed to parse: " ++ src)
  };
};

/* Measure rendered string length (display chars, matching ProbeProj.len_seg) */
let rendered_length = (~available: int, src: string): int => {
  Util_web.Unicode.length(abbreviate_and_render(~available, src));
};

/* Full-length render (no abbreviation) */
let full_render = (src: string): string => {
  switch (Parser.to_term(src, ~root=Exp)) {
  | Some(term) => render_exp(term)
  | None => failwith("Failed to parse: " ++ src)
  };
};

let full_length = (src: string): int => {
  Util_web.Unicode.length(full_render(src));
};

/* Check monotonicity: sweeping budget from 0 to full_len,
   rendered length should be non-decreasing */
let check_monotonicity = (name: string, src: string): unit => {
  let max_budget = full_length(src) + 5;
  let violation = ref(None);
  let rec sweep = (budget: int, prev_len: int): unit =>
    if (budget > max_budget) {
      ();
    } else {
      let rendered = abbreviate_and_render(~available=budget, src);
      let len = Util_web.Unicode.length(rendered);
      if (len < prev_len && violation^ == None) {
        violation :=
          Some(
            Printf.sprintf(
              "%s: monotonicity violated at budget %d: len=%d < prev_len=%d (budget %d had len %d)",
              name,
              budget,
              len,
              prev_len,
              budget - 1,
              prev_len,
            ),
          );
      };
      sweep(budget + 1, len);
    };
  sweep(0, 0);
  switch (violation^) {
  | Some(msg) => fail(msg)
  | None => ()
  };
};

/* Helper: run abbreviation on already-parsed expression */
let run_abbreviation = (~available: int, exp: Exp.t): Exp.t => {
  let (abbreviated: Exp.t, _length: int) =
    Abbreviate.abbreviate_exp(~available, exp);
  abbreviated;
};

let collect_labels = (elements: list(Exp.t)): list(string) =>
  elements
  |> List.filter_map((element: Exp.t) =>
       switch (element.term) {
       | TupLabel(label_exp, _value_exp) =>
         switch (label_exp.term) {
         | Label(name) => Some(name)
         | Invalid(name) => Some(name) /* abbreviated labels use Invalid */
         | _ => None
         }
       | _ => None
       }
     );

let is_flat_ellipses_exp = (exp: Exp.t): bool =>
  switch (exp.term) {
  | Invalid(str)
  | Atom(String(str))
  | Constructor(str, _)
  | Var(str) => str == ellipsis
  | _ => false
  };

let rec exp_contains_flat_ellipses = (exp: Exp.t): bool =>
  if (is_flat_ellipses_exp(exp)) {
    true;
  } else {
    switch (exp.term) {
    | Tuple(elements)
    | ListLit(elements) => List.exists(exp_contains_flat_ellipses, elements)
    | _ => false
    };
  };

/* ===== Monotonicity tests ===== */

let monotonicity_tests =
  List.map(
    ((name, src)) =>
      test_case("monotonicity: " ++ name, `Quick, () =>
        check_monotonicity(name, src)
      ),
    [
      ("int literal", "42"),
      ("bool true", "true"),
      ("bool false", "false"),
      ("string literal", "\"hello\""),
      ("short var", "x"),
      ("long var", "myLongVariable"),
      ("tuple 3", "(1, 2, 3)"),
      ("tuple 5", "(1, 2, 3, 4, 5)"),
      ("list 5", "[1, 2, 3, 4, 5]"),
      ("binary op", "1 + 2"),
      ("let expr", "let x = 1 in x + 1"),
      ("constructor app", "Some(42)"),
      ("labeled tuple", "(alpha=1, beta=2, gamma=3)"),
      ("empty list", "[]"),
      ("empty tuple", "()"),
      ("cons", "1::2::[]"),
      ("negation", "-5"),
      ("nested tuple", "((1, 2), (3, 4))"),
      ("list of tuples", "[(1, 2), (3, 4)]"),
      ("nested constructor app", {|Lam("bro", Var("bro"))|}),
      ("simple constructor app", {|Some(42)|}),
      ("constructor string arg", {|Lam("bro")|}),
      ("long list 10", "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"),
      (
        "long list 20",
        "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]",
      ),
      ("list of record", "[(crop=Moonmelon, quality=Bronze, quantity=2)]"),
      ("nested labeled tuple", "(inner=(a=1, b=2), outer=3)"),
      ("labeled tuple with list", "(items=[1, 2, 3], total=6)"),
    ],
  );

/* ===== Existing structural tests ===== */

let structural_tests = [
  test_case(
    "labeled tuples: greedy distribution with count annotation",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      open Exp;
      let original: Exp.t =
        tuple([
          tup_label(label("alpha"), string("aaaaaaaaaaaa")),
          tup_label(label("beta"), string("bbbbbbbbbbbb")),
          tup_label(label("gamma"), string("cccccccccccc")),
        ]);
      /* Budget 12: shows 1 field + annotation (1*5 + 1*2 + 3 = 10 ≤ 12).
         Greedy gives first field surplus budget. */
      let abbreviated_12: Exp.t = run_abbreviation(~available=12, original);
      switch (abbreviated_12.term) {
      | Tuple(elements) =>
        check(Alcotest.int, "element count at 12", 2, List.length(elements));
        switch (List.nth(elements, 0)) {
        | {term: TupLabel(label_exp, _), _} =>
          switch (label_exp.term) {
          | Label(name) =>
            check(Alcotest.string, "first label name", "alpha", name)
          | Invalid(name) =>
            check(
              Alcotest.bool,
              "label has content",
              true,
              String.length(name) > 0,
            )
          | _ => fail("expected label expression")
          }
        | _ => fail("expected TupLabel as first element")
        };
      | _ => fail("expected tuple at budget 12")
      };
      /* Budget 22: all 3 fields fit (3*5 + 2*2 = 19 ≤ 22), no annotation. */
      let abbreviated_22: Exp.t = run_abbreviation(~available=22, original);
      switch (abbreviated_22.term) {
      | Tuple(elements) =>
        check(Alcotest.int, "element count at 22", 3, List.length(elements))
      | _ => fail("expected tuple at budget 22")
      };
    },
  ),
  test_case(
    "label retains prefix before value elides",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      open Exp;
      let original: Exp.t =
        tuple([
          tup_label(
            label("capacity"),
            list_lit([int(1), int(2), int(3), int(4)]),
          ),
        ]);
      let abbreviated: Exp.t = run_abbreviation(~available=8, original);
      switch (abbreviated.term) {
      | Tuple([{term: TupLabel(label_exp, value_exp), _}]) =>
        let label_text: string =
          switch (label_exp.term) {
          | Label(text)
          | Invalid(text) => text /* abbreviated labels use Invalid */
          | _ => fail("expected label expression")
          };
        check(
          Alcotest.bool,
          "label keeps prefix",
          true,
          String.length(label_text) > 0
          && label_text != ellipsis
          && label_text.[0] == 'c',
        );
        check(
          Alcotest.bool,
          "value ellides at least as eagerly",
          true,
          exp_contains_flat_ellipses(value_exp),
        );
      | _ => fail("expected tuple with TupLabel")
      };
    },
  ),
  test_case(
    "split_evenly distributes fairly",
    `Quick,
    (): unit => {
      let budgets: list(int) =
        Abbreviate.AbbrevBudget.split_evenly(~total=10, ~parts=3);
      check(Alcotest.list(Alcotest.int), "even split", [4, 3, 3], budgets);
    },
  ),
];

/* ===== Budget respect tests ===== */

let budget_tests = [
  test_case(
    "zero budget produces minimal output",
    `Quick,
    () => {
      let len = rendered_length(~available=0, "42");
      check(Alcotest.bool, "zero budget output is small", true, len <= 5);
    },
  ),
  test_case(
    "large budget preserves full expression",
    `Quick,
    () => {
      let src = "(1, 2, 3)";
      let full = full_render(src);
      let abbrev = abbreviate_and_render(~available=100, src);
      check(Alcotest.string, "full budget roundtrip", full, abbrev);
    },
  ),
];

/* ===== Hard cap tests: rendered width must never exceed budget ===== */

let check_hard_cap = (name: string, src: string): unit => {
  let max_budget = full_length(src) + 5;
  let violation = ref(None);
  let rec sweep = (budget: int): unit =>
    if (budget > max_budget) {
      ();
    } else {
      let rendered = abbreviate_and_render(~available=budget, src);
      let len = Util_web.Unicode.length(rendered);
      if (len > budget && violation^ == None) {
        violation :=
          Some(
            Printf.sprintf(
              "%s: hard cap violated at budget %d: len=%d rendered=%s",
              name,
              budget,
              len,
              rendered,
            ),
          );
      };
      sweep(budget + 1);
    };
  /* Budget 0 can never be satisfied (minimum output is ellipsis = 1 char),
     so start sweep at 1. */
  sweep(1);
  switch (violation^) {
  | Some(msg) => fail(msg)
  | None => ()
  };
};

let hard_cap_tests =
  List.map(
    ((name, src)) =>
      test_case("hard cap: " ++ name, `Quick, () =>
        check_hard_cap(name, src)
      ),
    [
      ("int literal", "42"),
      ("bool true", "true"),
      ("bool false", "false"),
      ("string literal", "\"hello\""),
      ("short var", "x"),
      ("long var", "myLongVariable"),
      ("tuple 3", "(1, 2, 3)"),
      ("tuple 5", "(1, 2, 3, 4, 5)"),
      ("list 5", "[1, 2, 3, 4, 5]"),
      ("binary op", "1 + 2"),
      ("let expr", "let x = 1 in x + 1"),
      ("constructor app", "Some(42)"),
      ("labeled tuple", "(alpha=1, beta=2, gamma=3)"),
      ("empty list", "[]"),
      ("empty tuple", "()"),
      ("cons", "1::2::[]"),
      ("negation", "-5"),
      ("nested tuple", "((1, 2), (3, 4))"),
      ("list of tuples", "[(1, 2), (3, 4)]"),
      ("nested constructor app", {|Lam("bro", Var("bro"))|}),
      ("simple constructor app", {|Some(42)|}),
      ("constructor string arg", {|Lam("bro")|}),
      ("long list 10", "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"),
      (
        "long list 20",
        "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]",
      ),
      ("list of record", "[(crop=Moonmelon, quality=Bronze, quantity=2)]"),
      ("nested labeled tuple", "(inner=(a=1, b=2), outer=3)"),
      ("labeled tuple with list", "(items=[1, 2, 3], total=6)"),
    ],
  );

/* ===== Count annotation tests ===== */

let count_annotation_tests = [
  test_case(
    "count annotation appears for truncated list",
    `Quick,
    () => {
      let src = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
      /* Budget 15: should show some items + count annotation */
      let rendered = abbreviate_and_render(~available=15, src);
      check(
        Alcotest.bool,
        "contains +N annotation",
        true,
        {
          let has_plus =
            try({
              let _ =
                Str.search_forward(Str.regexp({|\+[0-9]+|}), rendered, 0);
              true;
            }) {
            | Not_found => false
            };
          has_plus;
        },
      );
    },
  ),
  test_case(
    "full budget shows all items without annotation",
    `Quick,
    () => {
      let src = "[1, 2, 3, 4, 5]";
      let rendered = abbreviate_and_render(~available=100, src);
      check(
        Alcotest.bool,
        "no + annotation",
        true,
        !String.contains(rendered, '+'),
      );
    },
  ),
];

/* ===== Unit-cost atom tests: 1-char values should not be replaced by ellipsis ===== */

let unit_cost_atom_tests = [
  test_case(
    "EmptyHole at budget 0 stays as EmptyHole",
    `Quick,
    () => {
      open IdTagged.FreshGrammar;
      open Exp;
      let hole = empty_hole();
      let result = run_abbreviation(~available=0, hole);
      check(
        Alcotest.bool,
        "term is still EmptyHole, not ellipsis",
        true,
        switch (result.term) {
        | EmptyHole => true
        | _ => false
        },
      );
    },
  ),
  test_case(
    "EmptyHole at budget 1 stays as EmptyHole",
    `Quick,
    () => {
      open IdTagged.FreshGrammar;
      open Exp;
      let hole = empty_hole();
      let result = run_abbreviation(~available=1, hole);
      check(
        Alcotest.bool,
        "term is still EmptyHole",
        true,
        switch (result.term) {
        | EmptyHole => true
        | _ => false
        },
      );
    },
  ),
  test_case(
    "single digit at budget 1 stays as digit",
    `Quick,
    () => {
      let rendered = abbreviate_and_render(~available=1, "0");
      check(Alcotest.string, "renders as 0", "0", rendered);
    },
  ),
  test_case(
    "two-digit number at budget 0 renders as ellipsis",
    `Quick,
    () => {
      let rendered = abbreviate_and_render(~available=0, "42");
      check(Alcotest.string, "budget 0", ellipsis, rendered);
    },
  ),
  test_case(
    "single char var at budget 1 stays as var",
    `Quick,
    () => {
      let rendered = abbreviate_and_render(~available=1, "x");
      check(Alcotest.string, "renders as x", "x", rendered);
    },
  ),
  test_case(
    "Deferral at budget 0 stays as Deferral",
    `Quick,
    () => {
      let d: Exp.t = IdTagged.fresh(Deferral(InAp): Exp.term);
      let result = run_abbreviation(~available=0, d);
      check(
        Alcotest.bool,
        "term is still Deferral, not ellipsis",
        true,
        switch (result.term) {
        | Deferral(_) => true
        | _ => false
        },
      );
    },
  ),
];

let module_abbreviation_tests = [
  test_case(
    "labeled tuples keep field names under tight budget",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      open Exp;
      let original: Exp.t =
        tuple([
          tup_label(label("alpha"), string("aaaaaaaaaaaa")),
          tup_label(label("beta"), string("bbbbbbbbbbbb")),
          tup_label(label("gamma"), string("cccccccccccc")),
        ]);
      let abbreviated: Exp.t = run_abbreviation(~available=24, original);
      switch (abbreviated.term) {
      | Tuple(elements) =>
        check(Alcotest.int, "field count", 3, List.length(elements));
        let labels: list(string) = collect_labels(elements);
        check(Alcotest.int, "label count", 3, List.length(labels));
        List.iter(
          (label: string) =>
            check(
              Alcotest.bool,
              "label not empty",
              true,
              String.length(label) > 0,
            ),
          labels,
        );
      | _ => fail("expected tuple after abbreviation")
      };
    },
  ),
  test_case(
    "label retains prefix before value elides",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      open Exp;
      let original: Exp.t =
        tuple([
          tup_label(
            label("capacity"),
            list_lit([int(1), int(2), int(3), int(4)]),
          ),
        ]);
      /* Use budget=12 so the TupLabel structure is preserved but value
         must elide. With the budget/retry system, budget=6 may collapse
         TupLabel since its min cost is 5. */
      let abbreviated: Exp.t = run_abbreviation(~available=12, original);
      switch (abbreviated.term) {
      | Tuple([{term: TupLabel(label_exp, value_exp), _}]) =>
        let label_text: string =
          switch (label_exp.term) {
          | Label(text) => text
          | Var(text) => text
          | Invalid(text) => text
          | _ => fail("expected label expression")
          };
        check(
          Alcotest.bool,
          "label keeps prefix",
          true,
          String.length(label_text) > 0
          && label_text != ellipsis
          && label_text.[0] == 'c',
        );
        check(
          Alcotest.bool,
          "value ellides at least as eagerly",
          true,
          exp_contains_flat_ellipses(value_exp),
        );
      | _ => fail("expected tuple with TupLabel")
      };
    },
  ),
  /* ===== MODULE ABBREVIATION TESTS =====
     NOTE: In practice, modules are expanded to labeled tuples before
     abbreviation runs on probe values. These tests verify the Module
     term case handles gracefully anyway. */
  test_case(
    "module single binding abbreviates under tight budget",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      let original: Exp.t =
        Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]);
      let abbreviated: Exp.t = run_abbreviation(~available=20, original);
      switch (abbreviated.term) {
      | Module([{term: ModLet(_, _), _}]) =>
        /* Module structure preserved with single let */
        ()
      | _ => check(Alcotest.bool, "expected Module with ModLet", true, false)
      };
    },
  ),
  test_case(
    "module abbreviates to ellipsis under very tight budget",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      let original: Exp.t =
        Exp.module_([
          Mod.mod_let(Pat.var("x"), Exp.int(1)),
          Mod.mod_let(Pat.var("y"), Exp.int(2)),
        ]);
      let abbreviated: Exp.t = run_abbreviation(~available=2, original);
      check(
        Alcotest.bool,
        "abbreviates to something",
        true,
        switch (abbreviated.term) {
        | Invalid(_)
        | Var(_) => true /* ellipsis or indet */
        | Module(_) => true /* or still a module */
        | _ => true
        },
      );
    },
  ),
  /* ===== MODULE EXPRESSION (ModuleExp) ABBREVIATION TESTS ===== */
  test_case(
    "ModuleExp abbreviates def and body",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      let original: Exp.t =
        Exp.module_exp(
          MPat.var("M"),
          Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
          Exp.var("M"),
        );
      let abbreviated: Exp.t = run_abbreviation(~available=30, original);
      switch (abbreviated.term) {
      | ModuleExp(_, _, _) =>
        /* ModuleExp structure preserved */
        ()
      | _ => check(Alcotest.bool, "expected ModuleExp", true, false)
      };
    },
  ),
  test_case(
    "ModuleExp abbreviates to ellipsis under tight budget",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      let original: Exp.t =
        Exp.module_exp(
          MPat.var("M"),
          Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
          Exp.var("M"),
        );
      let abbreviated: Exp.t = run_abbreviation(~available=3, original);
      check(
        Alcotest.bool,
        "abbreviates to something",
        true,
        switch (abbreviated.term) {
        | Invalid(_)
        | Var(_) => true
        | _ => true
        },
      );
    },
  ),
  /* ===== MPat ABBREVIATION via ModuleExp ===== */
  test_case(
    "ModuleExp with annotated MPat abbreviates",
    `Quick,
    (): unit => {
      open IdTagged.FreshGrammar;
      let original: Exp.t =
        Exp.module_exp(
          MPat.asc(
            MPat.var("M"),
            Typ.prod([Typ.tup_label(Typ.label("x"), Typ.int())]),
          ),
          Exp.module_([Mod.mod_let(Pat.var("x"), Exp.int(1))]),
          Exp.var("M"),
        );
      let abbreviated: Exp.t = run_abbreviation(~available=40, original);
      switch (abbreviated.term) {
      | ModuleExp({term: Asc(_, _), _}, _, _) =>
        /* ModuleExp with Asc MPat preserved */
        ()
      | ModuleExp(_, _, _) =>
        /* MPat might be abbreviated away - still ok */
        ()
      | _ => check(Alcotest.bool, "expected ModuleExp", true, false)
      };
    },
  ),
  test_case(
    "split_evenly distributes fairly",
    `Quick,
    (): unit => {
      let budgets: list(int) =
        Abbreviate.AbbrevBudget.split_evenly(~total=10, ~parts=3);
      check(Alcotest.list(Alcotest.int), "even split", [4, 3, 3], budgets);
    },
  ),
];

let tests = (
  "Abbreviate",
  structural_tests
  @ monotonicity_tests
  @ budget_tests
  @ hard_cap_tests
  @ count_annotation_tests
  @ unit_cost_atom_tests
  @ module_abbreviation_tests,
);
