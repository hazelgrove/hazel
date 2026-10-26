open Alcotest;
open Test_Evaluator_Prelude;
open Language.IdTagged.FreshGrammar;
open Exp;

let rat_value = (n: int, d: int) =>
  switch (Util.Rational.make(Bigint.of_int(n), Bigint.of_int(d))) {
  | Some(r) => basic(Rat(r))
  | None => Alcotest.fail("zero denominator in expected value")
  };

/* `use Rational` and the Rational expected type re-kind Int literals and
   operators exactly as `use Nat` / a Nat annotation do (D7 parity). */
let rational_tests = [
  test_case("use Rational: exact division", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|(use Rational in 1 / 3) == rat(1, 3)|},
    )
  ),
  test_case("use Rational: addition", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|(use Rational in 1 / 3 + 1 / 6) == rat(1, 2)|},
    )
  ),
  test_case("use Rational: subtraction", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|(use Rational in 1 / 3 - 1 / 2) == rat(-1, 6)|},
    )
  ),
  test_case("use Rational: multiplication", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|(use Rational in (2 / 3) * (3 / 4)) == rat(1, 2)|},
    )
  ),
  test_case(
    "use Rational: comparisons",
    `Quick,
    () => {
      parse_and_evaluate_test({|true|}, {|use Rational in 1 / 3 < 1 / 2|});
      parse_and_evaluate_test({|true|}, {|use Rational in 1 / 2 <= 2 / 4|});
      parse_and_evaluate_test(
        {|true|},
        {|use Rational in 2 / 3 > 1 / 2 && 1 / 2 >= 1 / 2|},
      );
      parse_and_evaluate_test({|false|}, {|use Rational in 1 / 2 < 1 / 3|});
    },
  ),
  test_case("use Rational: negation", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|(use Rational in -(1 / 3)) == rat(-1, 3)|},
    )
  ),
  test_case(
    "Rational annotation re-kinds a literal",
    `Quick,
    () => {
      parse_and_evaluate_test(
        {|true|},
        {|(let x : Rational = 5 in x) == rat(5, 1)|},
      );
      parse_and_evaluate_test(
        {|true|},
        {|(let x : Rational = -5 in x) == rat(-5, 1)|},
      );
    },
  ),
  test_case("use Rational: variable and literal mix", `Quick, () =>
    parse_and_evaluate_test(
      {|true|},
      {|let x = rat(1, 3) in (use Rational in x + 1 / 3) == rat(2, 3)|},
    )
  ),
  test_case(
    "Rational literal pattern",
    `Quick,
    () => {
      parse_and_evaluate_test(
        {|true|},
        {|case rat(2, 1) | 2 => true | _ => false end|},
      );
      parse_and_evaluate_test(
        {|false|},
        {|case rat(1, 2) | 1 => true | _ => false end|},
      );
    },
  ),
  test_case(
    "Explicit float operator stays Float",
    `Quick,
    () => {
      parse_and_evaluate_test({|true|}, {|(use Rational in 7 /. 2) ==. 3.5|});
      parse_and_evaluate_test({|true|}, {|(use Nat in 7 /. 2) ==. 3.5|});
    },
  ),
  test_case("Default Int division unchanged", `Quick, () =>
    parse_and_evaluate_test({|3|}, {|7 / 2|})
  ),
  test_case("Int-ascribed operand negates under use Rational", `Quick, () =>
    parse_and_evaluate_test(
      {|0|},
      {|let n = (use Rational in -3 : Int) in n + 3|},
    )
  ),
  test_case("use Rational: divide by zero", `Quick, () =>
    evaluation_test(
      "1 / 0",
      dynamic_error_hole(
        bin_op(Rat(Divide), rat_value(1, 1), rat_value(0, 1)),
        DivideByZero,
      ),
      elaborate(parse_exp({|use Rational in 1 / 0|})),
    )
  ),
  /* Power takes an Int exponent: the exponent literal, and its negation,
     stay Int inside `use Rational` (expected type beats the mode). */
  test_case(
    "use Rational: power with Int exponent",
    `Quick,
    () => {
      parse_and_evaluate_test(
        {|true|},
        {|(use Rational in (2 / 3) ** 2) == rat(4, 9)|},
      );
      parse_and_evaluate_test(
        {|true|},
        {|(use Rational in (2 / 3) ** -2) == rat(9, 4)|},
      );
      parse_and_evaluate_test(
        {|true|},
        {|let n = 3 in (use Rational in (1 / 2) ** n) == rat(1, 8)|},
      );
      parse_and_evaluate_test(
        {|true|},
        {|(use Rational in (1 / 2) ** 0) == rat(1, 1)|},
      );
    },
  ),
  test_case("use Rational: zero to a negative power", `Quick, () =>
    evaluation_test(
      "0 ** -1",
      dynamic_error_hole(
        bin_op(Rat(Power), rat_value(0, 1), int(-1)),
        DivideByZero,
      ),
      elaborate(parse_exp({|use Rational in 0 ** -1|})),
    )
  ),
  test_case("Int negative power unchanged", `Quick, () =>
    evaluation_test(
      "2 ** -1",
      dynamic_error_hole(
        bin_op(Int(Power), int(2), int(-1)),
        NegativeExponent,
      ),
      elaborate(parse_exp({|2 ** -1|})),
    )
  ),
  /* Static errors, as for Nat: binary operators ignore the expected type,
     float literals never re-kind, and Int values do not mix with a
     Rational operator. */
  ...List.map(
       ((name, s)) =>
         Test_Statics_Prelude.inconsistent_typecheck(
           name,
           Test_Statics_Prelude.parse_exp(s),
         ),
       [
         (
           "Rational annotation does not re-kind an operator",
           {|let x : Rational = 2 + 3 in x|},
         ),
         (
           "Rational annotation does not re-kind a float literal",
           {|let x : Rational = 0.5 in x|},
         ),
         (
           "Int value under a Rational operator",
           {|use Rational in string_length("ab") + 1|},
         ),
         (
           "Float literal under use Rational",
           {|use Rational in 1 / 3 + 0.5|},
         ),
         (
           "Rational exponent is rejected",
           {|use Rational in (1 / 2) ** (1 / 2)|},
         ),
       ],
     ),
];

let tests = (
  "Evaluator.Operators",
  [
    test_case("Integer sum", `Quick, () =>
      evaluation_test("4 + 5", int(9), bin_op(Int(Plus), int(4), int(5)))
    ),
    test_case("Negative integer literal", `Quick, () =>
      evaluation_test("-8", int(-8), un_op(Int(Minus), int(8)))
    ),
    /* Negation re-kinds by operand/analysis class (replace_un_op_cls):
       `-` on a float is float negation, not an Int type error. */
    test_case("Negative float literal, syn position", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|-1.5 ==. 0. -. 1.5|})
    ),
    test_case("Negation of float variable", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|let x = 2.5 in -x ==. 0. -. 2.5|})
    ),
    test_case("Float analysis retypes negated int literal", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|let x: Float = -5 in x ==. 0. -. 5.|},
      )
    ),
    test_case("Negative int still Int under negation", `Quick, () =>
      parse_and_evaluate_test({|0|}, {|-5 + 5|})
    ),
    test_case("Negation of parenthesized float expression", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|atan(0. -. 1.) ==. -(atan(1.))|})
    ),
    /* Under a `use` mode, a definitively-Int operand re-kinds negation
       back to Int (was a float op stuck on the Int operand). The `let`
       keeps the use-expression in synthetic position so the operand
       peek, not the ana branch, decides. */
    test_case("Int-ascribed operand negates under use Float", `Quick, () =>
      parse_and_evaluate_test(
        {|0|},
        {|let n = (use Float in -3 : Int) in n + 3|},
      )
    ),
    test_case("Bare literal under use Float still floats", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|let y = (use Float in -3) in y ==. 0. -. 3.|},
      )
    ),
    test_case("Inconsistent type ascription in subterm", `Quick, () =>
      parse_and_evaluate_test("1 + (4 : String)", {|1 + (4 : String)|})
    ),
    test_case(
      "Ascriptions around indet operators collapse",
      `Quick,
      () => {
        parse_and_evaluate_test("x + y", "(x + y) : Int");
        parse_and_evaluate_test("x - y", "(x - y) : Int");
        parse_and_evaluate_test("x * y", "(x * y) : Int");
        parse_and_evaluate_test("x ** y", "(x ** y) : Int");
        parse_and_evaluate_test("x / y", "(x / y) : Int");
        parse_and_evaluate_test("x < y", "(x < y) : Bool");
        parse_and_evaluate_test("x <= y", "(x <= y) : Bool");
        parse_and_evaluate_test("x > y", "(x > y) : Bool");
        parse_and_evaluate_test("x >= y", "(x >= y) : Bool");
        parse_and_evaluate_test(
          ~ignore_dynamic_errors=true,
          "x == y",
          "(x == y) : Bool",
        );
        parse_and_evaluate_test(
          ~ignore_dynamic_errors=true,
          "x != y",
          "(x != y) : Bool",
        );
        parse_and_evaluate_test("s1 ++ s2", "(s1 ++ s2) : String");
        parse_and_evaluate_test("s1 +. s2", "(s1 +. s2) : Float");
        parse_and_evaluate_test("s1 -. s2", "(s1 -. s2) : Float");
        parse_and_evaluate_test("s1 *. s2", "(s1 *. s2) : Float");
        parse_and_evaluate_test("s1 **. s2", "(s1 **. s2) : Float");
        parse_and_evaluate_test("s1 /. s2", "(s1 /. s2) : Float");
        parse_and_evaluate_test("b1 && b2", "(b1 && b2) : Bool");
        parse_and_evaluate_test("b1 || b2", "(b1 || b2) : Bool");
        parse_and_evaluate_test("b1 ==> b2", "(b1 ==> b2) : Bool");
      },
    ),
    test_case(
      "Material implication evaluates McCarthy left-to-right",
      `Quick,
      () => {
        /* A false antecedent short-circuits to true without evaluating
           the consequent — even past a domain error. */
        parse_and_evaluate_test("true", "false ==> 1 / 0 == 1");
        parse_and_evaluate_test("false", "true ==> false");
        parse_and_evaluate_test("true", "true ==> true");
        parse_and_evaluate_test("true", "false ==> false");
        /* Nested with other bool ops at the documented precedences:
           (a || b) ==> (c && d). */
        parse_and_evaluate_test("false", "false || true ==> true && false");
      },
    ),
  ]
  @ rational_tests,
);
