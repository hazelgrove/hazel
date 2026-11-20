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
          sint(Bigint.of_int(8)),
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
          asc(sint(Bigint.of_int(4)), Typ.string()),
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
        evaluation_test(
          "SInt",
          sint(Bigint.of_int(4)),
          elaborate(parse_exp("4 : SInt")),
        );
        evaluation_test(
          "Nat",
          nat(Bigint.of_int(4)),
          elaborate(parse_exp("4 : Nat")),
        );
      },
    ),
    /* An SInt has 64 bits everywhere: a result past them is an error, IntegerOverflow, as a division by zero is one,
       where it wrapped before (at 32 bits in the browser, 63 natively) */
    test_case("SInt overflow is an error", `Quick, () =>
      List.iter(
        ~f=
          ((name, src)) => {
            let shown =
              String.map(
                ~f=c => Char.equal(c, '\n') ? ' ' : c,
                Language.Exp.show(parse_and_evaluate(src)),
              );
            let has = part =>
              Str.string_match(
                Str.regexp(".*" ++ Str.quote(part)),
                shown,
                0,
              );
            check(
              Alcotest.bool,
              name
              ++ ": "
              ++ (
                String.length(shown) > 600
                  ? String.sub(shown, ~pos=0, ~len=600) : shown
              ),
              true,
              has("DynamicErrorHole") && has("IntegerOverflow"),
            );
          },
        [
          ("a sum", "use SInt in 9223372036854775807 + 1"),
          ("a difference", "use SInt in 0 - 9223372036854775807 - 2"),
          ("a product", "use SInt in 4294967296 * 4294967296"),
          ("a power", "use SInt in 2 ** 63"),
          ("a power far past 64 bits", "use SInt in 3 ** 1000000"),
          (
            "a negation",
            "use SInt in let x = 0 - 9223372036854775807 - 1 in -x",
          ),
          (
            "the least SInt divided by -1",
            "use SInt in (0 - 9223372036854775807 - 1) / (0 - 1)",
          ),
        ],
      )
    ),
    test_case("SInt within 64 bits, at their edges", `Quick, () =>
      parse_and_evaluate_test(
        "(true, true, true, true, true, true, true)",
        {|use SInt in (9223372036854775806 + 1 == 9223372036854775807, 3037000499 * 3037000499 == 9223372030926249001, 2 ** 62 == 4611686018427387904, (0 - 9223372036854775807 - 1) / 2 == 0 - 4611686018427387904, 0 - 9223372036854775807 - 1 < 0, (let y = 0 - 9223372036854775807 in -y) == 9223372036854775807, 2147483647 + 1 == 2147483648)|},
      )
    ),
    /* A power of 64 or more is not computed (Operators.sint_power): -1, 0 and 1 repeat, anything else overflows */
    test_case("SInt powers of -1, 0 and 1 past 64", `Quick, () =>
      parse_and_evaluate_test(
        "(true, true, true, true)",
        {|use SInt in ((0 - 1) ** 1000001 == 0 - 1, (0 - 1) ** 1000000 == 1, 0 ** 1000000 == 0, 1 ** 1000000 == 1)|},
      )
    ),
    test_case("The overflow error says an SInt has 64 bits", `Quick, () =>
      check(
        Alcotest.string,
        "IntegerOverflow's message",
        "Error: Integer overflow (an SInt has 64 bits)",
        InvalidOperationError.err_msg(InvalidOperationError.IntegerOverflow),
      )
    ),
    /* A Float is truncated toward zero first; the Floats nearest 2^63 are 2^63 - 1024 and -2^63 */
    test_case("A value within 64 bits is made into an SInt", `Quick, () =>
      parse_and_evaluate_test(
        "(true, true, true, true, true, true)",
        {|(int_of_sint(sint_of_int(0 - 9223372036854775808)) == 0 - 9223372036854775808, int_of_sint(sint_of_nat(9223372036854775807 : Nat)) == 9223372036854775807, int_of_sint(sint_of_float(9223372036854774784.)) == 9223372036854774784, int_of_sint(sint_of_float(0. -. 9223372036854775808.)) == 0 - 9223372036854775808, int_of_sint(sint_of_float(2.9)) == 2, int_of_sint(sint_of_float(0. -. 2.9)) == 0 - 2)|},
      )
    ),
    test_case("max_sint and min_sint are the 64-bit edges", `Quick, () =>
      parse_and_evaluate_test(
        "(true, true)",
        {|(int_of_sint(max_sint) == 9223372036854775807, int_of_sint(min_sint) == 0 - 9223372036854775808)|},
      )
    ),
    /* A conversion that does not fit stays a call, as a builtin's other errors do; before, a Float wrapped
       (sint_of_float(3000000000.) was -1294967296 in the browser) */
    test_case("A value outside 64 bits is not made into an SInt", `Quick, () =>
      List.iter(
        ~f=
          src => {
            let shown =
              String.map(
                ~f=c => Char.equal(c, '\n') ? ' ' : c,
                Language.Exp.show(parse_and_evaluate(src)),
              );
            let has = part =>
              Str.string_match(
                Str.regexp(".*" ++ Str.quote(part)),
                shown,
                0,
              );
            check(
              Alcotest.bool,
              src ++ " stays a call, no SInt: " ++ shown,
              true,
              has("BuiltinFun") && !has("Atom (SInt"),
            );
          },
        [
          "sint_of_int(9223372036854775808)",
          "sint_of_int(0 - 9223372036854775809)",
          "sint_of_nat(9223372036854775808 : Nat)",
          "sint_of_float(9223372036854775808.)",
          "sint_of_float(0. -. 9223372036854777856.)",
          "sint_of_float(1. /. 0.)",
        ],
      )
    ),
  ],
);
