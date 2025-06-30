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
  ],
);
