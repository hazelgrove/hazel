open Alcotest;
open Haz3lcore;
open Language;

let ellipsis = "…";

/* Settings matching what ProbeProj uses for rendering abbreviated terms */
let abbrev_settings: ExpToSegment.Settings.t = {
  ...ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
  show_unknown_as_hole: false,
};

let exp_to_seg = ExpToSegment.exp_to_segment(~settings=abbrev_settings);
let seg_to_str = Printer.of_segment(~holes="?", ~indent="");

/* Render an expression to string using the same pipeline as ProbeProj */
let render_exp = (exp: Exp.t): string => {
  exp |> exp_to_seg |> seg_to_str;
};

/* Parse source string, abbreviate at given budget, render to string */
let abbreviate_and_render = (~available: int, src: string): string => {
  switch (Parser.to_term(src)) {
  | Some(term) =>
    let (abbreviated, _length) = Abbreviate.abbreviate_exp(~available, term);
    render_exp(abbreviated);
  | None => failwith("Failed to parse: " ++ src)
  };
};

/* Measure rendered string length (display chars, matching ProbeProj.len_seg) */
let rendered_length = (~available: int, src: string): int => {
  Util.Unicode.length(abbreviate_and_render(~available, src));
};

/* Full-length render (no abbreviation) */
let full_render = (src: string): string => {
  switch (Parser.to_term(src)) {
  | Some(term) => render_exp(term)
  | None => failwith("Failed to parse: " ++ src)
  };
};

let full_length = (src: string): int => {
  Util.Unicode.length(full_render(src));
};

/* Debug: show the abbreviated AST structure for a TupLabel tuple */
let debug_labeled_tuple_ast = (~available: int, src: string): unit => {
  switch (Parser.to_term(src)) {
  | Some(term) =>
    let (abbreviated, _length) = Abbreviate.abbreviate_exp(~available, term);
    Printf.printf("  [AST budget=%d] ", available);
    switch (abbreviated.term) {
    | Tuple(elements) =>
      Printf.printf("Tuple([");
      List.iteri(
        (i, el: Exp.t) => {
          if (i > 0) {
            Printf.printf("; ");
          };
          switch (el.term) {
          | TupLabel(lab, value) =>
            let lab_str =
              switch (lab.term) {
              | Label(s) => "Label(\"" ++ s ++ "\")"
              | Invalid(s) => "Invalid(\"" ++ s ++ "\")"
              | _ => "?"
              };
            let val_str =
              switch (value.term) {
              | Atom(Int(n)) => "Atom(Int(" ++ Bigint.to_string(n) ++ "))"
              | Atom(SInt(n)) => "Atom(SInt(" ++ string_of_int(n) ++ "))"
              | Invalid(s) => "Invalid(\"" ++ s ++ "\")"
              | Var(s) => "Var(\"" ++ s ++ "\")"
              | _ => "other"
              };
            Printf.printf("TL(%s, %s)", lab_str, val_str);
          | Invalid(s) => Printf.printf("Invalid(\"%s\")", s)
          | _ => Printf.printf("?")
          };
        },
        elements,
      );
      Printf.printf("])\n");
    | Parens({term: Tuple(elements), _}) =>
      Printf.printf("Parens(Tuple([");
      List.iteri(
        (i, el: Exp.t) => {
          if (i > 0) {
            Printf.printf("; ");
          };
          switch (el.term) {
          | TupLabel(lab, value) =>
            let lab_str =
              switch (lab.term) {
              | Label(s) => "Label(\"" ++ s ++ "\")"
              | Invalid(s) => "Invalid(\"" ++ s ++ "\")"
              | _ => "?"
              };
            let val_str =
              switch (value.term) {
              | Atom(Int(n)) => "Atom(Int(" ++ Bigint.to_string(n) ++ "))"
              | Atom(SInt(n)) => "Atom(SInt(" ++ string_of_int(n) ++ "))"
              | Invalid(s) => "Invalid(\"" ++ s ++ "\")"
              | Var(s) => "Var(\"" ++ s ++ "\")"
              | _ => "other"
              };
            Printf.printf("TL(%s, %s)", lab_str, val_str);
          | Invalid(s) => Printf.printf("Invalid(\"%s\")", s)
          | _ => Printf.printf("?")
          };
        },
        elements,
      );
      Printf.printf("]))\n");
    | Invalid(s) => Printf.printf("Invalid(\"%s\")\n", s)
    | _ => Printf.printf("other: term constructor\n")
    };
  | None => Printf.printf("  parse failed\n")
  };
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
      let len = Util.Unicode.length(rendered);
      let byte_len = String.length(rendered);
      Printf.printf(
        "[%s] budget=%d  unicode_len=%d  byte_len=%d  rendered=%s\n",
        name,
        budget,
        len,
        byte_len,
        rendered,
      );
      if (name == "labeled tuple" && budget >= 27 && budget <= 31) {
        debug_labeled_tuple_ast(~available=budget, src);
      };
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
    ],
  );

/* ===== Existing structural tests ===== */

let structural_tests = [
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
      let abbreviated: Exp.t = run_abbreviation(~available=28, original);
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
      let abbreviated: Exp.t = run_abbreviation(~available=8, original);
      switch (abbreviated.term) {
      | Tuple([{term: TupLabel(label_exp, value_exp), _}]) =>
        let label_text: string =
          switch (label_exp.term) {
          | Label(text) => text
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
      let len = Util.Unicode.length(rendered);
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
    ],
  );

let tests = (
  "Abbreviate",
  structural_tests @ monotonicity_tests @ budget_tests @ hard_cap_tests,
);
