open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Modules",
  [
    test_case("Module with multiple bindings evaluates in order", `Quick, () =>
      parse_and_evaluate_test(
        {|{val x = 3 * 6 ;; val y = 2 + 5 }|},
        {| { val x = 18 ;; val y = 7 } |}
      )
    ),
    test_case("Later bindings can reference earlier ones in module", `Quick, () =>
      parse_and_evaluate_test(
        " { val x = 3 ;; val y = 5 } ",
        {| { val x = 3 ;; val y = x + 2 } |}
      )
    ),
        test_case("Module with type definition preserves type def", `Quick, () =>
      parse_and_evaluate_test(
        {| { type T = Int ;; val x = 3 } |},
        {| { type T = Int ;; val x : T = 3 } |}
      )
    ),
    test_case("Module with type ascription applies to binding", `Quick, () =>
      parse_and_evaluate_test(
        {|{ type T = Int ;; val x = "hello" : Int }|},
        {| { type T = Int ;; val x : T = "hello" } |}
      )
    ),
  ],
);
