open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.OptionType",
  [
    test_case("Option type is available in context", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|None|},
      )
    }),
    test_case("Option type Some constructor", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(42)|},
        {|Some(42)|},
      )
    }),
    test_case("Option type pattern matching", `Quick, () => {
      parse_and_evaluate_test(
        {|42|},
        {|case Some(42) | None => 0 | Some(x) => x end|},
      )
    }),
    test_case("option_map with Some value", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(84)|},
        {|option_map(Some(42), fun x -> x * 2)|},
      )
    }),
    test_case("option_map with None", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|option_map(None, fun x -> x * 2)|},
      )
    }),
    test_case("option_bind with Some value", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(84)|},
        {|option_bind(Some(42), fun x -> Some(x * 2))|},
      )
    }),
    test_case("option_bind with None", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|option_bind(None, fun x -> Some(x * 2))|},
      )
    }),
    test_case("option_bind with function returning None", `Quick, () => {
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|option_bind(Some(42), fun x -> None)|},
      )
    }),
    test_case("option_to_list with Some value", `Quick, () => {
      parse_and_evaluate_test({|[42]|}, {|option_to_list(Some(42))|})
    }),
    test_case("option_to_list with None", `Quick, () => {
      parse_and_evaluate_test({|[]|}, {|option_to_list(None)|})
    }),
  ],
);
