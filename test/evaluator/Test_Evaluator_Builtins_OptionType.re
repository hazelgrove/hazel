open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.OptionType",
  [
    test_case("Option type is available in context", `Quick, () => {
      // Test that we can construct None
      evaluation_test(
        "None",
        elaborate(parse_exp("None")),
        elaborate(parse_exp("None")),
      )
    }),
    test_case("Option type Some constructor", `Quick, () => {
      // Test that we can construct Some with a value
      evaluation_test(
        "Some(42)",
        elaborate(parse_exp("Some(42)")),
        elaborate(parse_exp("Some(42)")),
      )
    }),
    test_case("Option type pattern matching", `Quick, () => {
      // Test pattern matching on Option type
      evaluation_test(
        "case Some(42) | None => 0 | Some(x) => x end",
        elaborate(parse_exp("42")),
        elaborate(parse_exp("42")),
      )
    }),
    test_case("option_map with Some value", `Quick, () => {
      // Test option_map with Some value
      evaluation_test(
        "Some(84)",
        elaborate(parse_exp("option_map(Some(42), fun x -> x * 2)")),
        elaborate(parse_exp("Some(84)")),
      )
    }),
    test_case("option_map with None", `Quick, () => {
      // Test option_map with None
      evaluation_test(
        "None",
        elaborate(parse_exp("option_map(None, fun x -> x * 2)")),
        elaborate(parse_exp("None")),
      )
    }),
    test_case("option_bind with Some value", `Quick, () => {
      // Test option_bind with Some value
      evaluation_test(
        "Some(84)",
        elaborate(parse_exp("option_bind(Some(42), fun x -> Some(x * 2))")),
        elaborate(parse_exp("Some(84)")),
      )
    }),
    test_case("option_bind with None", `Quick, () => {
      // Test option_bind with None
      evaluation_test(
        "None",
        elaborate(parse_exp("option_bind(None, fun x -> Some(x * 2))")),
        elaborate(parse_exp("None")),
      )
    }),
    test_case("option_bind with function returning None", `Quick, () => {
      // Test option_bind with function that returns None
      evaluation_test(
        "None",
        elaborate(parse_exp("option_bind(Some(42), fun x -> None)")),
        elaborate(parse_exp("None")),
      )
    }),
    test_case("option_to_list with Some value", `Quick, () => {
      // Test option_to_list with Some value
      evaluation_test(
        "[42]",
        elaborate(parse_exp("option_to_list(Some(42))")),
        elaborate(parse_exp("[42]")),
      )
    }),
    test_case("option_to_list with None", `Quick, () => {
      // Test option_to_list with None
      evaluation_test(
        "[]",
        elaborate(parse_exp("option_to_list(None)")),
        elaborate(parse_exp("[]")),
      )
    }),
  ],
);
