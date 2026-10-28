open Alcotest;
open Language;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.LabeledTuples",
  [
    test_case(
      "Automatic label insertion in pattern for labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let x : (a=Int) -> Int = fun a -> a in x(2)|},
      )
    ),
    test_case("Ascription removal for labels in let pattern", `Quick, () => {
      parse_and_evaluate_test("(a=1)", {|let x : (a=Int) = (a=1) in x|})
    }),
    /* Both entries are kept. The second `a` is not field `b`, so its cast
       to `b`'s type stays rather than relabeling the value. */
    /* An empty label matches any label, so the cast is pushed in. */
    test_case("A cast to a label pushes into a wildcard label", `Quick, () =>
      evaluation_test(
        "(=1) : (a=Int)",
        tup_label(label(""), int(1)),
        asc(
          tup_label(label(""), int(1)),
          Typ.tup_label(Typ.label("a"), Typ.int()),
        ),
      )
    ),
    test_case("Repeated label ascribed to a labeled tuple type", `Quick, () =>
      check(
        dhexp_typ,
        "((a=1, a=2) : (a=Int, b=Int))",
        tuple([
          tup_label(label("a"), int(1)),
          asc(
            tup_label(label("a"), int(2)),
            Typ.tup_label(Typ.label("b"), Typ.int()),
          ),
        ]),
        parse_and_evaluate({|((a=1, a=2) : (a=Int, b=Int))|}),
      )
    ),
    test_case("Labeled tuple field access", `Quick, () =>
      parse_and_evaluate_test("1", {|(a=1,b=2).a|})
    ),
    test_case("Anonymous function with explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
  fun (a=a : String) -> string_length(a)
in fn("hello")|},
      )
    }),
    test_case("Anonymous function without explicit label", `Quick, () => {
      parse_and_evaluate_test(
        "5",
        {|let fn : (a=String) -> Int =
            fun (a : String) -> string_length(a)
          in fn("hello")|},
      )
    }),
    /* A field whose label differs from the type's keeps its cast, labels
       and all: the value doesn't have that type, so it isn't relabelled
       (`(a=3 : Bool, b="" : Float)`). */
    test_case("Inconsistent labels", `Quick, () => {
      evaluation_test(
        "(a=3, b=\"\") : (c=Bool, d=Float)",
        tuple([
          asc(
            tup_label(label("a"), int(3)),
            Typ.tup_label(Typ.label("c"), Typ.bool()),
          ),
          asc(
            tup_label(label("b"), string("")),
            Typ.tup_label(Typ.label("d"), Typ.float()),
          ),
        ]),
        elaborate(parse_exp({|(a=3, b="") : (c=Bool, d=Float)|})),
      )
    }),
    test_case("Dot operation for missing label", `Quick, () =>
      parse_and_evaluate_test("(a=1,b=2).c", "(a=1,b=2).c")
    ),
    test_case(
      "Unlabeled pattern binds a labeled element's value, not its label (#2070)",
      `Quick,
      () =>
      parse_and_evaluate_test("3", {|let (a,b,c) = (?=1, 2, b=3) in c|})
    ),
    test_case("Desructuring labeled tuple", `Quick, () =>
      parse_and_evaluate_test(
        "(1, 2, 3.0)",
        {|let (a=a', b=b', c) = (a=1, b=2, 3.0) in (a',b',c)|},
      )
    ),
    test_case("Labeled tuple projection", `Quick, () =>
      evaluation_test(
        "(a=1, b=2, c=?).a",
        int(1),
        dot(
          tuple([
            tup_label(label("a"), int(1)),
            tup_label(label("b"), int(2)),
            tup_label(label("c"), empty_hole()),
          ]),
          label("a"),
        ),
      )
    ),
    test_case("hole field projection", `Quick, () =>
      parse_and_evaluate_test("?.a", "?.a")
    ),
    test_case(
      "Indet projection",
      `Quick,
      () => {
        parse_and_evaluate_test("(true) . a", "(true) . a");
        parse_and_evaluate_test("((true) . a): Int", "((true) . a): Int");
        parse_and_evaluate_test(
          ~msg="Duplicate labels projected are indet",
          "(a=1, a=2).a",
          {|(a=1, a=2).a|},
        );
      },
    ),
    test_case(
      "Extension",
      `Quick,
      () => {
        parse_and_evaluate_test("(a=1,b=2,c=3)", {|(a=1, b=2) ... (c=3)|});
        parse_and_evaluate_test("(1,2,3,4)", {|(1, 2) ... (3, 4)|});
        parse_and_evaluate_test(
          "(a=1, b=2, 3, c=4)",
          {|(a=0, b=2) ... (a=1, 3, c=4)|},
        );
      },
    ),
    test_case("labeled tuple multi-label selection", `Quick, () =>
      parse_and_evaluate_test(
        "(a=1, b=2)",
        {|select_labels((a=1,b=2,c=3), `a`, `b`)|},
      )
    ),
    test_case("labeled tuple multi-label projection", `Quick, () =>
      parse_and_evaluate_test(
        "(3, 1, 3)",
        {|project_labels((a=1,b=2,c=3), `c`, `a`, `c`)|},
      )
    ),
    test_case("Omit labels", `Quick, () =>
      parse_and_evaluate_test(
        "(c=3)",
        {|omit_labels((a=1,b=2,c=3), `a`, `b`)|},
      )
    ),
    test_case("Omit all labels", `Quick, () =>
      parse_and_evaluate_test(
        "(1,2,3,4)",
        {|omit_all_labels((a=1,b=2,3,c=4))|},
      )
    ),
    test_case("Explicitly unlabeled tuple entry", `Quick, () =>
      parse_and_evaluate_test("1", {|let (_=x) = (_=1) in x|})
    ),
  ],
);
