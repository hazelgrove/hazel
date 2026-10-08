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
  ],
);
