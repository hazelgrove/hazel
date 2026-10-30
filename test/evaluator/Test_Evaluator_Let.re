open Alcotest;
open Test_Evaluator_Prelude;
let tests = (
  "Evaluator.Let",
  [
    test_case("Inconsisent type ascription on let", `Quick, () =>
      parse_and_evaluate_test("(4 : String)", {|let x : String = 4  in x|})
    ),
    /* ===== Function-definition sugar evaluates correctly =====
       `let f(args) = body` must produce the same value as the explicit
       `let f = fun (args) -> body` form. */
    test_case("function sugar: basic binary function", `Quick, () =>
      parse_and_evaluate_test(
        "7",
        {|let f(x: Int, y: Int) = x + y in f(3, 4)|},
      )
    ),
    test_case("function sugar: return-type annotation", `Quick, () =>
      parse_and_evaluate_test(
        "7",
        {|let f(x: Int, y: Int): Int = x + y in f(3, 4)|},
      )
    ),
    test_case("function sugar: single argument", `Quick, () =>
      parse_and_evaluate_test("42", {|let inc(x) = x + 1 in inc(41)|})
    ),
    test_case("function sugar: curried arguments", `Quick, () =>
      parse_and_evaluate_test("7", {|let add(x)(y) = x + y in add(3)(4)|})
    ),
    test_case("function sugar: recursive factorial", `Quick, () =>
      parse_and_evaluate_test(
        "120",
        {|let fact(n: Int): Int = if n == 0 then 1 else n * fact(n - 1) in fact(5)|},
      )
    ),
    test_case("function sugar: curried, leading nullary group", `Quick, () =>
      parse_and_evaluate_test("3", {|let f()(x) = x + 1 in f()(2)|})
    ),
    test_case("function sugar: curried, trailing nullary group", `Quick, () =>
      parse_and_evaluate_test("10", {|let f(x)() = x * 2 in f(5)()|})
    ),
    test_case("function sugar: curried, three groups in order", `Quick, () =>
      parse_and_evaluate_test(
        "123",
        {|let f(a)(b)(c) = a * 100 + b * 10 + c in f(1)(2)(3)|},
      )
    ),
    test_case("function sugar: curried, partial application", `Quick, () =>
      parse_and_evaluate_test(
        "11",
        {|let f(x)(y) = x + y in let g = f(1) in g(10)|},
      )
    ),
    test_case("function sugar: curried, recursive", `Quick, () =>
      parse_and_evaluate_test(
        "10",
        {|let sum(n: Int)(acc: Int): Int = if n == 0 then acc else sum(n - 1)(acc + n) in sum(4)(0)|},
      )
    ),
    test_case("function sugar: curried, pattern arguments", `Quick, () =>
      parse_and_evaluate_test(
        "6",
        {|let f((a, b))(c) = a + b + c in f((1, 2))(3)|},
      )
    ),
    test_case("function sugar: curried, module member", `Quick, () =>
      parse_and_evaluate_test(
        "3",
        {|module M = { let f(x)(y) = x + y } in M.f(1)(2)|},
      )
    ),
  ],
);
