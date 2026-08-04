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
          "Real decimal arithmetic stays symbolic",
          bin_op(
            Operators.Real(Plus),
            real(Real.of_decimal("1.25")),
            real(Real.of_decimal("2.5")),
          ),
          elaborate(parse_exp({|use Real in 1.25 + 2.5|})),
        );
        evaluation_test(
          "Real division stays symbolic",
          bin_op(
            Operators.Real(Divide),
            real(Real.of_bigint(Bigint.one)),
            real(Real.of_bigint(Bigint.of_int(3))),
          ),
          elaborate(parse_exp({|use Real in 1 / 3|})),
        );
        evaluation_test(
          "Real division by zero stays symbolic",
          bin_op(
            Operators.Real(Divide),
            real(Real.of_bigint(Bigint.one)),
            real(Real.of_bigint(Bigint.zero)),
          ),
          elaborate(parse_exp({|use Real in 1 / 0|})),
        );
        evaluation_test(
          "Real comparison stays symbolic",
          bin_op(
            Operators.Real(LessThan),
            real(Real.of_bigint(Bigint.one)),
            real(Real.of_bigint(Bigint.of_int(2))),
          ),
          elaborate(parse_exp({|use Real in 1 < 2|})),
        );
        evaluation_test(
          "Real negation stays symbolic",
          un_op(Operators.Real(Minus), real(Real.of_decimal("1.25"))),
          elaborate(parse_exp({|use Real in -1.25|})),
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
