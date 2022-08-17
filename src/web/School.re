open Virtual_dom.Vdom;
open Node;

//TODO: make sure in the final version hidden editors are NOT PLAINTEXT STRINGS

type cell = {
  caption: string,
  chapter: option(Node.t),
  initial: string,
};

/* NOTE: num_editors here should agree with TestView.school_panel */
let defaults: list(cell) = [
  {
    caption: "Your Implementation",
    initial: "let odd: Int->Bool = in ",
    chapter:
      Some(
        div(
          [],
          [
            text(
              "Implement the function 'odd' in the 'Your Implementation' cell. Odd only needs to support
              non-negative integers as input, and should produce a boolean as output. Note that there is
              no 'not' in Hazel, nor a way to compare booleans for equality, so you may want to implement
              one or both of these yourself. The name and type signature of the 'odd' function must remain
              the same, but the return value of the cell is irrelevant.",
            ),
            br([]),
            br([]),
            text(
              "Add enough tests to the 'Your Tests' section below to reveal all bugs in our buggy implementations.
              The syntax for a test is 'test <boolean-expression> end'. You can use semicolons (sequencing operator)
              to cleanly seperate tests. ",
            ),
            br([]),
            br([]),
            text(
              "Known Limitations: Concave holes in your input may result in limited interpretation.
              If you aren't getting test feedback on the right-hand side, it may be because your program has a type error.
              Nonempty holes aren't drawn yet, so you'll need to move the caret through the program while watching the
              cursor inspector in the top left. Test feedback isn't currently very helpful beyond pass/fail.
              If you experience any issues other than the limitations described here, please report
              them on slack!",
            ),
          ],
        ),
      ),
  },
  {caption: "Your Tests", initial: "", chapter: None},
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

let captions = List.map((cell: cell) => cell.caption, defaults);
let chapters = List.map((cell: cell) => cell.chapter, defaults);

let init: Model.school =
  defaults |> List.map((s: cell) => s.initial) |> Model.editors_of_strings;

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
