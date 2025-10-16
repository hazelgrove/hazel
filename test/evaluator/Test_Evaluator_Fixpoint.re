open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Fixpoints",
  [
    test_case("Inconsistent type in fixpoint pattern", `Quick, () =>
      parse_and_evaluate_test("[] : ()", {|fix () -> []|})
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
    test_case("Fixpoint pattern ascription", `Quick, () =>
      parse_and_evaluate_test(
        "() : Bool",
        {|fix (_ : Bool) -> test false end|},
      )
    ),
    test_case("Fixpoint with type ascription", `Quick, () =>
      parse_and_evaluate_test("() : Int", {|fix 0 -> ()|})
    ),
    test_case("Substitution", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let t =  1 in
          let go =
            fun i ->
          if i > 1 then t else go(i+1) in
          go(0)|},
      )
    ),
    test_case("Mutual recursion substitution", `Quick, () =>
      parse_and_evaluate_test(
        "1",
        {|let t =  1 in
          let (go, go') =
            (fun i ->
          if i > 3 then t else go'(i+1),fun i -> if i > 3 then t else go(i+1) ) in
          go(0)|},
      )
    ),
    test_case("Fixpoint that evaluates to the correct form", `Quick, () =>
      parse_and_evaluate_test("(1, 2)", {|let x = (1, 2) in fix f -> x|})
    ),
  ],
);
