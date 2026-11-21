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
    /* Labels stack: a tuple expected at a one-field label becomes that
       field, rather than the label being refused for arity. */
    test_case("Wide tuple lifted into a singleton label", `Quick, () =>
      parse_and_evaluate_test("(a=(1, 2))", {|(1, 2) : (a=(Int, Int))|})
    ),
    test_case("Wide tuple pattern lifted into a singleton label", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let f : (a=(Int, Int)) -> Int = fun (x, y) -> x in f((1, 2))|},
      )
    ),
    /* A value that already has the labeled type must not be lifted into a
       further field: the label is in its type, not in its syntax. */
    test_case(
      "Ascribing a variable that already has the record type", `Quick, () =>
      parse_and_evaluate_test("(a=1)", {|let r = (a=1) in r : (a=Int)|})
    ),
    test_case(
      "Annotating a variable that already has the record type", `Quick, () =>
      parse_and_evaluate_test(
        "(a=1)",
        {|let r = (a=1) in let x : (a=Int) = r in x|},
      )
    ),
    test_case(
      "Ascribing a call that already returns the record type", `Quick, () =>
      parse_and_evaluate_test(
        "(a=1)",
        {|let f = fun _ -> (a=1) in f(()) : (a=Int)|},
      )
    ),
    /* Of unknown type, it may still be the record at runtime: the cast
       decides, so it isn't wrapped again. */
    test_case(
      "Ascribing a variable of unknown type that is the record", `Quick, () =>
      parse_and_evaluate_test("(a=1)", {|let r : ? = (a=1) in r : (a=Int)|})
    ),
    test_case(
      "Annotating a variable of unknown type that is the record", `Quick, () =>
      parse_and_evaluate_test(
        "(a=1)",
        {|let r : ? = (a=1) in let x : (a=Int) = r in x|},
      )
    ),
    test_case(
      "Ascribing a call of unknown type that returns the record", `Quick, () =>
      parse_and_evaluate_test(
        "(a=1)",
        {|let f : ? -> ? = fun x -> (a=x) in f(1) : (a=Int)|},
      )
    ),
    /* One that isn't the record is lifted when the cast or the pattern
       meets it, as it would have been by its type. */
    test_case(
      "Annotating a variable of unknown type that isn't the record", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let v : ? = 1 in let r : (a=Int) = v in r.a|},
      )
    ),
    test_case(
      "Matching a variable of unknown type that isn't the record", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let f = fun (y : ?) -> let (a=x) = y in x in f(1)|},
      )
    ),
    test_case(
      "Matching a case scrutinee of unknown type that isn't the record",
      `Quick,
      () =>
      parse_and_evaluate_test(
        "1",
        {|let f = fun (y : ?) -> case y | (a=x) => x | _ => 0 end in f(1)|},
      )
    ),
    test_case("Ascribing a record of other labels of unknown type", `Quick, () =>
      parse_and_evaluate_test(
        "(a=(b=1))",
        {|let v : ? = (b=1) in v : (a=(b=Int))|},
      )
    ),
    /* A function value is a closure: the whole closure becomes the field,
       or the record is stuck inside it. */
    test_case("Lifting a function of unknown type", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let v : ? = fun z -> z + 1 in let r : (a=Int -> Int) = v in r.a(1)|},
      )
    ),
    test_case("Matching a lifted function of unknown type", `Quick, () =>
      parse_and_evaluate_test(
        "2",
        {|let v : ? = fun z -> z + 1 in let r : (a=Int -> Int) = v in case r | (a=g) => g(1) end|},
      )
    ),
    test_case("Ascribing a tuple of unknown type", `Quick, () =>
      parse_and_evaluate_test(
        "(a=(1, 2))",
        {|let v : ? = (1, 2) in v : (a=(Int, Int))|},
      )
    ),
    /* Statics lifts at a one-field labeled type, not at a recursive type
       that unrolls to one, which would lift without end. */
    test_case("A cast to a recursive labeled type doesn't lift", `Quick, () =>
      parse_and_evaluate_test(
        "1 : rec t -> (a=t)",
        {|let v : ? = 1 in v : rec t -> (a=t)|},
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
       and all: the value doesn't have that type. (This used to relabel
       silently, giving `(a=3 : Bool, b="" : Float)`.) */
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
