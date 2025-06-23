open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Modules",
  [
    test_case("Module with multiple bindings evaluates", `Quick, () =>
      parse_and_evaluate_test(
        {|{val x = 3 * 6 ;; val y = 2 + 5 }|},
        {| { val x = 18 ;; val y = 7 } |},
      )
    ),
    test_case(
      "Later bindings can reference earlier ones in module", `Quick, () =>
      parse_and_evaluate_test(
        " { val x = 3 ;; val y = 5 } ",
        {| { val x = 3 ;; val y = x + 2 } |},
      )
    ),
    test_case("Module with type definition preserves type def", `Quick, () =>
      parse_and_evaluate_test(
        {| { type T = Int ;; val x = 3 } |},
        {| { type T = Int ;; val x : T = 3 } |},
      )
    ),
    test_case("Module with type ascription applies to binding", `Quick, () =>
      parse_and_evaluate_test(
        {|{ type T = Int ;; val x = "hello" : Int }|},
        {| { type T = Int ;; val x : T = "hello" } |},
      )
    ),
    test_case(
      "Module bindings evaluate in order (first to last) with computation",
      `Quick,
      () => {
        let initial =
          elaborate(
            parse_exp(
              {|{ val x = 3 * 2 ;; val y = x + 4 ;; val z = y * 2 }|},
            ),
          );
        let expected_steps = [
          parse_exp({|{ val x = 6 ;; val y = x + 4 ;; val z = y * 2 }|}),
          parse_exp({|{ val x = 6 ;; val y = 6 + 4 ;; val z = y * 2 }|}),
          parse_exp({|{ val x = 6 ;; val y = 10 ;; val z = y * 2 }|}),
          parse_exp({|{ val x = 6 ;; val y = 10 ;; val z = 10 * 2 }|}),
          parse_exp({|{ val x = 6 ;; val y = 10 ;; val z = 20 }|}),
        ];
        assert_steps(
          ~msg="Module bindings evaluate first to last with computation",
          initial,
          expected_steps,
        );
      },
    ),
  ],
);
