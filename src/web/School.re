open Virtual_dom.Vdom;
open Node;

//TODO: make sure in the final version hidden editors are NOT PLAINTEXT STRINGS

type cell = {
  caption: string,
  chapter: option(Node.t),
  initial: string,
  hidden: bool,
};

/* NOTE: num_editors here should agree with TestView.school_panel */
let defaults: list(cell) = [
  {
    caption: "Student Implementation",
    initial: "let not:Bool->Bool=
fun x -> if x then false else true
in
let odd: Int->Bool =
fun x->
if x == 0
then false
else not(odd(x-1))
in",
    chapter:
      Some(
        div(
          [],
          [
            text(
              "Welcome to School mode! Clicking 'school' at the top toggles back to either a single editor
              or an editor per-page; clicking the toggle switches to the student sub-mode, hiding some of the
              below cells. In School mode, there are a fixed number of cells displayed in a vertical
              column. To add or remove cells go to School.re. If you want changes in the number or order
              of cells to be wired up to the dynamics, you'll also need to modify school_panel in TestView.re.
              Each cell has a text caption, a boolean flag to say whether it should be hidden in the student
              sub-mode, and can be optionally preceeded by arbitrary HTML like this section right here.",
            ),
          ],
        ),
      ),
    hidden: false,
  },
  {
    caption: "Student Tests",
    initial: "test not(odd(4)) end;
test odd(3) end;
test odd(5) end",
    chapter: None,
    hidden: false,
  },
  {
    caption: "Hidden Tests",
    initial: "test not(odd(0)) end;
test odd(1) end",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Reference Implementation",
    initial: "let a = true in
let not: Bool->Bool =
fun x -> if x then false else true
in
let odd:Int->Bool =
fun x ->
if x == 0
then false
else not(odd(x-1))
in yo",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Wrong Implementation 1",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> false
in 1",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Wrong Implementation 2",
    initial: "let not: Bool->Bool =
fun x -> if x then false else true in
let odd: Int->Bool =
fun x -> true
in 2",
    chapter: None,
    hidden: true,
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
if x == 5 then false else odd_correct(x)
in 3",
    chapter: None,
    hidden: true,
  },
];

let captions = List.map((cell: cell) => cell.caption, defaults);
let chapters = List.map((cell: cell) => cell.chapter, defaults);
let hiddens = List.map((cell: cell) => cell.hidden, defaults);

let init: Model.school =
  defaults |> List.map((s: cell) => s.initial) |> Model.editors_of_strings;
