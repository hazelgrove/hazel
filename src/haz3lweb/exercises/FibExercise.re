open Virtual_dom.Vdom;
open Node;
open ExerciseUtil;

let exercise: SchoolExercise.spec = {
  title: "Recursive Fibonacci",
  version: 1,
  point_distribution: {
    test_validation: 10,
    mutation_testing: 40,
    impl_grading: 50,
  },
  prompt:
    div([
      p([
        div([
          text(
            "Write tests cases for, and then implement, a function, that recursively determines the nth fibonacci number.",
          ),
        ]),
      ]),
      p([
        code("fib n"),
        text(" should return the "),
        code("n"),
        text("th fibonacci number, assuming "),
        code("n >= 0."),
      ]),
    ]),
  prelude: "",
  correct_impl: "let fib: Int -> Int =
fun x ->
if x < 2 then 1
else fib(x - 1) + fib(x - 2) in ",
  your_tests: {
    minimum: 0,
    num_required: 5,
    tests: "",
  },
  your_impl: "let fib: Int -> Int =
fun n -> in",
  hidden_bugs: [
    {
      impl: "let fib: Int -> Int =
fun x ->
if x < 1 then 0
else if x < 2 then 1
else fib(x - 1) + fib(x - 2)
in ",
      hint: "incorrect base case",
    },
    {
      impl: "let fib: Int -> Int =
fun x ->
if x < 2 then 1
else fib(x - 2) + fib(x - 2)
in",
      hint: "skipped too far",
    },
  ],
  hidden_tests: {
    tests: "test fib(0) == 1 end;
test fib(1) == 1 end;
test fib(2) == 2 end;
test fib(3) == 3 end;
test fib(4) == 5 end;
test fib(5) == 8 end;
test fib(6) == 13 end;
test fib(7) == 21 end;
test fib(8) == 34 end;",
    hints: ["base case 0", "base case 1"],
  },
  next_id: 0,
};
