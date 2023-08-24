open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type test = {
  name: string,
  sketch: string,
  options: FillerOptions.t,
};

let sketch_one = {|let update: (Model, Action) -> Model =
  FILL_ME
in
EXPORT|};

let opt = (~expected_type): FillerOptions.t => {
  llm: Azure_GPT4,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type,
  error_round: true,
};

let tests_raw: list(test) = [
  {name: "1", sketch: sketch_one, options: opt(~expected_type=true)},
  {name: "2", sketch: sketch_one, options: opt(~expected_type=true)},
  {name: "3", sketch: sketch_one, options: opt(~expected_type=true)},
  {name: "4", sketch: sketch_one, options: opt(~expected_type=true)},
  {name: "5", sketch: sketch_one, options: opt(~expected_type=false)},
  {name: "6", sketch: sketch_one, options: opt(~expected_type=false)},
  {name: "7", sketch: sketch_one, options: opt(~expected_type=false)},
  {name: "8", sketch: sketch_one, options: opt(~expected_type=false)},
];

[@deriving (show({with_path: false}), sexp, yojson)]
type single_run = (FillerOptions.t, list(UpdateAction.t));

let mk_script = (~options, ~sketch: string): single_run => {
  let sketch_slide = 4;
  (
    options,
    [
      Reset,
      SwitchScratchSlide(sketch_slide),
      PerformAction(Move(Extreme(Up))),
      PerformAction(Select(Resize(Extreme(Down)))),
      Paste(sketch),
      PerformAction(Move(Goal(Piece(FillMarker, Left)))),
      PerformAction(Select(Term(Current))),
      Paste(Form.expliciter_hole),
      PerformAction(Select(Term(Current))),
      Assistant(Prompt(Filler(options))),
    ],
  );
};

let test_scripts: list((string, single_run)) =
  List.map(
    ({name, sketch, options}) => (name, mk_script(~sketch, ~options)),
    tests_raw,
  );

/*
 TODO(andrew):
 - log if we send errors back
 - backup to localstore

 */
