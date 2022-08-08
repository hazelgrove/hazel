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
    initial: "let a = 2 in
let b = 3 in b",
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
    initial: "test a == b - 1 end;
test a > 0 end",
    chapter: None,
    hidden: false,
  },
  {
    caption: "Hidden Tests",
    initial: "test a == 2 end",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Reference Implementation",
    initial: "let a = 2 in let b = 3 in b",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Wrong Implementation 1",
    initial: "let a = 0 in a",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Wrong Implementation 2",
    initial: "let a = 1 in let b = 1 in a",
    chapter: None,
    hidden: true,
  },
  {
    caption: "Wrong Implementation 3",
    initial: "let a = 2 in let b = 2 in 65",
    chapter: None,
    hidden: true,
  },
];

let captions = List.map((cell: cell) => cell.caption, defaults);
let chapters = List.map((cell: cell) => cell.chapter, defaults);
let hiddens = List.map((cell: cell) => cell.hidden, defaults);

let init: Model.school =
  defaults |> List.map((s: cell) => s.initial) |> Model.editors_of_strings;
