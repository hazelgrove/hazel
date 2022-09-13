open Virtual_dom.Vdom;
open Node;

let exercise: SchoolExercise.spec = {
  next_id: 0,
  point_distribution: {
    test_validation: 10,
    mutation_testing: 40,
    impl_grading: 50,
  },
  prompt:
    div([
      text(
        "Write tests cases for, and then implement, the function filter_odds : [Int] -> [Int]
              that returns a list of all the odd elements of the input list. The order of elements in the input list should be preserved.",
      ),
      br(),
      br(),
      text(
        "You will need to implement two simple helper functions, odd and not, first.
              Test cases for these are provided below.",
      ),
    ]),
  prelude: "",
  correct_impl: "let not: Bool->Bool =
fun x -> if x then false else true
in
let odd:Int->Bool =
fun x ->
if x == 0
then false
else not(odd(x-1))
in yo",
  your_tests: {
    num_required: 4,
    tests: "test not(false) end;
test not(not(true)) end;
test not(odd(0)) end;
test odd(1) end;
test not(odd(2)) end;
test odd(11) end;
test not(odd(12)) end;
test list_eq(filter_odds(1::2::3::4::5::nil), 1::3::5::nil) end;",
  },
  your_impl: "let not: Bool -> Bool =
fun x -> in
let odd: Int -> Bool =
fun x -> in
let filter_odds : [Int] -> [Int] =
fun xs ->
in ",
  hidden_bugs: [
    {
      impl: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> false
in 1",
      hint: "always returns false",
    },
    {
      impl: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> true
in 2",
      hint: "always returns true",
    },
    {
      impl: "let not: Bool->Bool =
fun x -> if x then false else true
in let odd_correct:Int->Bool =
fun x -> if x == 0
then false
else not(odd(x-1))
in let odd: Int->Bool =
fun x ->
if x == 2 then true else odd_correct(x)
in 3",
      hint: "correct, except for 2",
    },
  ],
  hidden_tests: {
    tests: "test list_eq(filter_odds(nil), nil) end;
test list_eq(filter_odds(1::1::2::1::nil), 1::1::1::nil) end;
test list_eq(filter_odds(2::4::6::8::10::nil), nil) end",
    hints: ["base case", "duplicates", "evens only"],
  },
};
