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
        evaluation_test(
          "Real integer",
          real(Real.of_bigint(Bigint.of_int(8))),
          elaborate(parse_exp({|use Real in 8|})),
        );
        evaluation_test(
          "Real exact decimal",
          real(Real.normalize(Bigint.of_int(15), Bigint.of_int(4), None)),
          elaborate(parse_exp({|use Real in 1.25 + 2.5|})),
        );
        evaluation_test(
          "Real decimal arithmetic does not round through Float",
          real(Real.normalize(Bigint.of_int(3), Bigint.of_int(10), None)),
          elaborate(parse_exp({|use Real in 0.1 + 0.2|})),
        );
        evaluation_test(
          "Real non-terminating division",
          real(Real.normalize(Bigint.one, Bigint.of_int(3), None)),
          elaborate(parse_exp({|use Real in 1 / 3|})),
        );
        evaluation_test(
          "Real negative exponent",
          real(Real.normalize(Bigint.one, Bigint.of_int(8), None)),
          elaborate(parse_exp({|use Real in 2 ** -3|})),
        );
        evaluation_test(
          "Real pi constant",
          real(Real.Pi),
          elaborate(parse_exp({|pi_real|})),
        );
        evaluation_test(
          "Negated pi remains symbolic",
          un_op(Operators.Real(Minus), real(Real.Pi)),
          elaborate(parse_exp({|use Real in -pi_real|})),
        );
        parse_and_evaluate_test(
          "true",
          {|use Real in case 1.25 | 1.25 => true | _ => false end|},
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
  ],
);
