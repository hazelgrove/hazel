open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Fixpoints",
  [
    test_case("Inconsistent type in fixpoint pattern", `Quick, () =>
      parse_and_evaluate_test("fix () -> []", {|fix () -> []|})
    ),
  ],
);
