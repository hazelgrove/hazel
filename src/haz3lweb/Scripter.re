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

let sketch_two = {|let add: Model -> [Todo] =
fun (description, todos) ->
if description $== ""
  then todos
    else (description, false) :: todos in
    let remove: (Int, [Todo]) -> [Todo]=
fun (index, todos) ->
List.filteri(fun i, _ -> i!= index, todos) in
  let toggle: (Int, [Todo]) -> [Todo]=
fun (index, todos) ->
List.mapi(
  fun i, (description, done) ->
    (description, if i == index then !done else done),
      todos) in

let update: (Model, Action) -> Model =
   FILL_ME
  in
EXPORT|};

let opt = (~expected_type): FillerOptions.t => {
  params: OpenAI.default_params,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type,
  error_rounds_max: 2,
  relevant_ctx: false,
  rag: None,
};

let opt2 = (~instructions, ~syntax_notes, ~num_examples): FillerOptions.t => {
  params: OpenAI.default_params,
  instructions,
  syntax_notes,
  num_examples,
  expected_type: true,
  error_rounds_max: 2,
  relevant_ctx: false,
  rag: None,
};

let common_sketch = sketch_one;

let tests_raw: list(test) = [
  {
    name: "1a",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1b",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1c",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1d",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1e",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1f",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1g",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1h",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1i",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "1j",
    sketch: common_sketch,
    options: opt2(~instructions=true, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2a",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2b",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2c",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2d",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2e",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2f",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2g",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2h",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2i",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
  {
    name: "2j",
    sketch: common_sketch,
    options: opt2(~instructions=false, ~syntax_notes=true, ~num_examples=9),
  },
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
