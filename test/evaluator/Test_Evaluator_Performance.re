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
    /* The evaluator runs on a trampoline so that deep recursion in the
       program does not become deep recursion in JavaScript: each call
       here takes several JS frames if run directly, so 20,000 deep would
       overflow node's stack. Guards changes to the trampoline itself. */
    test_case("deep recursion runs on the trampoline", `Slow, () => {
      parse_and_evaluate_test(
        "20000",
        {|let count = fun n -> if n == 0 then 0 else 1 + count(n - 1) in
          count(20000)|},
      )
    }),
    test_case("a long list is built and walked on the trampoline", `Slow, () => {
      parse_and_evaluate_test(
        "20000",
        {|let build = fun n -> if n == 0 then [] else n :: build(n - 1) in
          let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in
          len(build(20000))|},
      )
    }),
  ],
);
