//TODO: make sure in the final version hidden editors are NOT PLAINTEXT STRINGS

module type Exercise = {
  let cells: list(SchoolCell.t);
  let hidden_test_descriptions: list(string);
  let wrong_implementation_descriptions: list(string);
};

/* NOTE: num_editors here should agree with TestView.school_panel */
module TheExercise: Exercise = FilterOddsExercise;

let captions =
  List.map((cell: SchoolCell.t) => cell.caption, TheExercise.cells);
let chapters =
  List.map((cell: SchoolCell.t) => cell.chapter, TheExercise.cells);

let init: Model.school =
  TheExercise.cells
  |> List.map((s: SchoolCell.t) => s.initial)
  |> Model.editors_of_strings;
