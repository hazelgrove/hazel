open Alcotest;
open Language;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

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
    /* An SInt has 32 bits everywhere: a result past them is an error, IntegerOverflow, as a division by zero is one,
       where it wrapped before (at 32 bits in the browser, 63 natively) */
    test_case("SInt overflow is an error", `Quick, () =>
      List.iter(
        ((name, src)) => {
          let shown =
            String.map(
              c => c == '\n' ? ' ' : c,
              Language.Exp.show(parse_and_evaluate(src)),
            );
          let has = part =>
            Str.string_match(Str.regexp(".*" ++ Str.quote(part)), shown, 0);
          check(
            Alcotest.bool,
            name
            ++ ": "
            ++ (
              String.length(shown) > 600 ? String.sub(shown, 0, 600) : shown
            ),
            true,
            has("DynamicErrorHole") && has("IntegerOverflow"),
          );
        },
        [
          ("a sum", "use SInt in 2147483647 + 1"),
          ("a difference", "use SInt in 0 - 2147483647 - 2"),
          ("a product", "use SInt in 65536 * 65536"),
          ("a power", "use SInt in 2 ** 31"),
          (
            "the least SInt divided by -1",
            "use SInt in (0 - 2147483647 - 1) / (0 - 1)",
          ),
        ],
      )
    ),
    test_case("SInt within 32 bits, at their edges", `Quick, () =>
      parse_and_evaluate_test(
        "(true, true, true, true, true)",
        {|use SInt in (2147483646 + 1 == 2147483647, 46340 * 46340 == 2147395600, 2 ** 30 == 1073741824, (0 - 2147483647 - 1) / 2 == 0 - 1073741824, 0 - 2147483647 - 1 < 0)|},
      )
    ),
    test_case(
      "An Int too big for an SInt is not made into one",
      `Quick,
      () => {
        let shown =
          String.map(
            c => c == '\n' ? ' ' : c,
            Language.Exp.show(parse_and_evaluate("sint_of_int(3000000000)")),
          );
        check(
          Alcotest.bool,
          "sint_of_int(3000000000) stays a call, no SInt: " ++ shown,
          false,
          Str.string_match(Str.regexp(".*Atom (SInt"), shown, 0),
        );
      },
    ),
  ],
);
