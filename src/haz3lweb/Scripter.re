open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type test = {
  name: string,
  sketch: string,
  options: FillerOptions.t,
};

let tests_raw: list(test) = [
  {
    name: "one",
    sketch: {|let update: (Model, Action) -> Model =
  FILL_ME
in
%EXPORT|},
    options: FillerOptions.init,
  },
  /*
   {
     name: "two",
     sketch: "let lol = FILL_ME in lol + 666",
     llm: Azure_GPT3_5Turbo,
     prompt_builder: Filler.prompt(~settings,~ctx_init),
   },*/
];

let mk_script = (~options, ~sketch: string): list(UpdateAction.t) => {
  [
    Reset,
    SwitchScratchSlide(5),
    PerformAction(Move(Extreme(Up))),
    PerformAction(Select(Resize(Extreme(Down)))),
    Paste(sketch),
    PerformAction(Move(Goal(Piece(FillMarker, Left)))),
    PerformAction(Select(Term(Current))),
    Paste(Form.expliciter_hole),
    PerformAction(Select(Term(Current))),
    Assistant(Prompt(Filler(options))),
  ];
};

let test_scripts =
  List.map(
    ({name, sketch, options}) => (name, mk_script(~sketch, ~options)),
    tests_raw,
  );

/*
 TODO(andrew):
 - log if we send errors back
 - backup to localstore

 */
