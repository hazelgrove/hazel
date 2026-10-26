open Alcotest;
open Language;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let rat_value = (n: string, d: string) =>
  switch (Util.Rational.make(Bigint.of_string(n), Bigint.of_string(d))) {
  | Some(r) => basic(Rat(r))
  | None => Alcotest.fail("zero denominator in expected value")
  };

let tests = (
  "Evaluator.Atom",
  [
    test_case("Integer literal", `Quick, () =>
      evaluation_test("8", int(8), int(8))
    ),
    test_case(
      "Use",
      `Quick,
      () => {
        evaluation_test(
          "Nat",
          nat(Bigint.of_int(8)),
          elaborate(parse_exp({|use Nat in 8|})),
        );
        evaluation_test(
          "SInt",
          sint(8),
          elaborate(parse_exp({|use SInt in 8|})),
        );
      },
    ),
    test_case(
      "Inconsistent type ascription",
      `Quick,
      () => {
        parse_and_evaluate_test("4 : String", {|4 : String|});
        parse_and_evaluate_test("true : String", {|true : String|});
        parse_and_evaluate_test("1. : String", {|1. : String|});
        evaluation_test(
          "Nat",
          asc(nat(Bigint.of_int(4)), Typ.string()),
          elaborate(parse_exp({|4 : Nat : String|})),
        );
        evaluation_test(
          "SInt",
          asc(sint(4), Typ.string()),
          elaborate(parse_exp({|4 : SInt : String|})),
        );
      },
    ),
    test_case(
      "Consistent type ascription",
      `Quick,
      () => {
        parse_and_evaluate_test("4", {|4 : Int|});
        parse_and_evaluate_test({|"hello"|}, {|"hello" : String|});
        parse_and_evaluate_test("true", {|true : Bool|});
      },
    ),
    test_case(
      "Type-directed",
      `Quick,
      () => {
        evaluation_test("SInt", sint(4), elaborate(parse_exp("4 : SInt")));
        evaluation_test(
          "Nat",
          nat(Bigint.of_int(4)),
          elaborate(parse_exp("4 : Nat")),
        );
      },
    ),
    test_case(
      "Rational constructor normalizes",
      `Quick,
      () => {
        evaluation_test(
          "rat(1, 3)",
          rat_value("1", "3"),
          elaborate(parse_exp("rat(1, 3)")),
        );
        evaluation_test(
          "lowest terms",
          rat_value("1", "3"),
          elaborate(parse_exp("rat(2, 6)")),
        );
        evaluation_test(
          "sign moves to numerator",
          rat_value("-1", "3"),
          elaborate(parse_exp("rat(2, -6)")),
        );
        evaluation_test(
          "two negatives",
          rat_value("1", "3"),
          elaborate(parse_exp("rat(-1, -3)")),
        );
        evaluation_test(
          "zero is 0/1",
          rat_value("0", "1"),
          elaborate(parse_exp("rat(0, -5)")),
        );
        evaluation_test(
          "integer value",
          rat_value("2", "1"),
          elaborate(parse_exp("rat(4, 2)")),
        );
      },
    ),
    test_case(
      "Rational constructor beyond 2^53",
      `Quick,
      () => {
        /* 2^65 / 6 = 2^64 / 3, and (2^53 + 1) / 1 is not a float */
        evaluation_test(
          "2^65 / 6",
          rat_value("18446744073709551616", "3"),
          elaborate(parse_exp("rat(36893488147419103232, 6)")),
        );
        evaluation_test(
          "2^53 + 1",
          rat_value("9007199254740993", "1"),
          elaborate(parse_exp("rat(9007199254740993, 1)")),
        );
        evaluation_test(
          "large cancellation",
          rat_value("1", "3"),
          elaborate(
            parse_exp(
              "rat(12345678901234567890123, 37037036703703703670369)",
            ),
          ),
        );
      },
    ),
    test_case(
      "Rational constructor with zero denominator",
      `Quick,
      () => {
        evaluation_test(
          "rat(1, 0)",
          dynamic_error_hole(
            ap(Forward, builtin_fun("rat"), tuple([int(1), int(0)])),
            DivideByZero,
          ),
          elaborate(parse_exp("rat(1, 0)")),
        );
        evaluation_test(
          "rat(0, 0)",
          dynamic_error_hole(
            ap(Forward, builtin_fun("rat"), tuple([int(0), int(0)])),
            DivideByZero,
          ),
          elaborate(parse_exp("rat(0, 0)")),
        );
      },
    ),
    test_case(
      "Rational type ascription",
      `Quick,
      () => {
        evaluation_test(
          "annotated let",
          rat_value("1", "2"),
          elaborate(parse_exp("let x : Rational = rat(3, 6) in x")),
        );
        evaluation_test(
          "function over Rational",
          rat_value("-1", "2"),
          elaborate(
            parse_exp(
              "let f : Rational -> Rational = fun x -> x in f(rat(1, -2))",
            ),
          ),
        );
      },
    ),
    test_case(
      "Rational equality",
      `Quick,
      () => {
        evaluation_test(
          "equivalent fractions",
          bool(true),
          elaborate(parse_exp("rat(1, 3) == rat(2, 6)")),
        );
        evaluation_test(
          "different values",
          bool(false),
          elaborate(parse_exp("rat(1, 3) == rat(1, 2)")),
        );
        evaluation_test(
          "not equal",
          bool(false),
          elaborate(parse_exp("rat(1, 3) != rat(-2, -6)")),
        );
        evaluation_test(
          "sign",
          bool(false),
          elaborate(parse_exp("rat(1, 3) == rat(-1, 3)")),
        );
        evaluation_test(
          "zeros",
          bool(true),
          elaborate(parse_exp("rat(0, 7) == rat(0, -2)")),
        );
        evaluation_test(
          "beyond 2^53",
          bool(true),
          elaborate(
            parse_exp(
              "rat(36893488147419103232, 6) == rat(18446744073709551616, 3)",
            ),
          ),
        );
        evaluation_test(
          "beyond 2^53, off by one",
          bool(false),
          elaborate(
            parse_exp("rat(9007199254740993, 1) == rat(9007199254740992, 1)"),
          ),
        );
        evaluation_test(
          "inside tuples and lists",
          bool(true),
          elaborate(
            parse_exp(
              "(rat(1, 2), [rat(2, 4), rat(3, 1)]) == (rat(-1, -2), [rat(1, 2), rat(6, 2)])",
            ),
          ),
        );
        evaluation_test(
          "inside tuples and lists, one differs",
          bool(false),
          elaborate(
            parse_exp(
              "(rat(1, 2), [rat(2, 4), rat(3, 1)]) == (rat(1, 2), [rat(1, 2), rat(3, 2)])",
            ),
          ),
        );
      },
    ),
  ],
);
