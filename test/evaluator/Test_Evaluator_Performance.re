/* These are very simple tests to make sure we're not
   doing exponential blowup in the evaluator */

open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.Performance",
  [
    test_case("lists don't accumulate from patterns", `Slow, () => {
      parse_and_evaluate_test(
        "100",
        {|let list_of_ones = fun n ->
            if n == 0 then [] else 1 :: list_of_ones(n-1)
          in

          let sum = fun l:[Int] ->
            case l
              | [] => 0
              | x::xs => x + sum(xs)
            end
          in

          let n = 100 in

          sum(list_of_ones(n))|},
      )
    }),
  ],
);
