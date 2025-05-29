open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Fixpoints",
  [
    test_case("Inconsistent type in fixpoint pattern", `Quick, () =>
      parse_and_evaluate_test("fix () -> [] : ()", {|fix () -> []|})
    ),
    test_case("Fixpoint with boolean short circuiting", `Quick, () =>
      parse_and_evaluate_test("true", {|fix f -> true || f|})
    ),
    test_case("Fixpoint with function", `Quick, () =>
      parse_and_evaluate_test(
        "3",
        {|(fix f -> fun i -> if i > 2 then i else f(i+1))(1)|},
      )
    ),
    test_case("Mutual recursion", `Quick, () =>
      parse_and_evaluate_test(
        "false",
        {|let (even, odd) = (fix (even, odd) -> (fun n -> if n == 0 then true else odd(n - 1),
fun n -> if n == 0 then false else even(n - 1))) in even(1)|},
      )
    ),
    test_case("Fixpoint with wildcard unit type", `Quick, () =>
      parse_and_evaluate_test("()", {|fix _ : () -> test B end|})
    ),
    test_case("Fixpoint with wrong arity", `Quick, () =>
      parse_and_evaluate_test(
        "fix (a,b,c) -> (a,b) : (?, ?, ?)",
        {|fix (a,b,c) -> (a,b)|},
      )
    ),
    test_case("Fixpoint pattern ascription", `Quick, () =>
      parse_and_evaluate_test(
        "() : Bool",
        {|fix (_ : Bool) -> test false end|},
      )
    ),
    test_case("Fixpoint with type ascription", `Quick, () =>
      parse_and_evaluate_test("fix 0 -> () : Int", {|fix 0 -> ()|})
    ),
    test_case("Wild statics fixpoint list cons", `Quick, () =>
      parse_and_evaluate_test("(1: [?]):: (1: [?])", {|fix(_:: x) -> x :: 1|})
    ),
  ],
);
