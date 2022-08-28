open Virtual_dom.Vdom;
open Node;

let cells: list(SchoolCell.t) = [
  {
    caption: "Your Implementation",
    initial: "let not: Bool -> Bool =
fun x -> in
let odd: Int -> Bool =
fun x -> in
let filter_odds : [Int] -> [Int] =
fun xs ->
in ",
    chapter:
      Some(
        div(
          [],
          [
            text(
              "Write tests cases for, and then implement, the function filter_odds : [Int] -> [Int]
              that returns a list of all the odd elements of the input list. The order of elements in the input list should be preserved.",
            ),
            br([]),
            br([]),
            text(
              "You will need to implement two simple helper functions, odd and not, first.
              Test cases for these are provided below.",
            ),
          ],
        ),
      ),
  },
  {
    caption: "Your Tests",
    initial: "test not(false) end;
test not(not(true)) end;
test not(odd(0)) end;
test odd(1) end;
test not(odd(2)) end;
test odd(11) end;
test not(odd(12)) end;
test list_eq(filter_odds(1::2::3::4::5::nil), 1::3::5::nil) end;",
    chapter: None,
  },
  {
    caption: "Hidden Tests",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true
in
test not(odd(0)) end;
test odd(1) end",
    chapter: None,
  },
  {
    caption: "Reference Implementation",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true
in
let odd:Int->Bool =
fun x ->
if x == 0
then false
else not(odd(x-1))
in yo",
    chapter: None,
  },
  {
    caption: "Wrong Implementation 1",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> false
in 1",
    chapter: None,
  },
  {
    caption: "Wrong Implementation 2",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> true
in 2",
    chapter: None,
  },
  {
    caption: "Wrong Implementation 3",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true
in let odd_correct:Int->Bool =
fun x -> if x == 0
then false
else not(odd(x-1))
in let odd: Int->Bool =
fun x ->
if x == 2 then true else odd_correct(x)
in 3",
    chapter: None,
  },
];

let hidden_test_descriptions = [
  "Check base case",
  "Simplest recursive case",
  "Second-simplest recursive case",
];

let wrong_implementation_descriptions = [
  "Always returns true",
  "Always returns false",
  "Correct, except for 2",
];
