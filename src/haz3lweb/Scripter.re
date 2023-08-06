open Haz3lcore;
open Sexplib.Std;

let sketch_slide = 7;
let fill_marker = "FILL_ME";

[@deriving (show({with_path: false}), sexp, yojson)]
type test = {
  name: string,
  sketch: string,
  llm: OpenAI.chat_models,
  prompt_builder: Editor.t => option(OpenAI.prompt),
};

let tests_raw = (~ctx_init): list(test) => [
  {
    name: "one",
    sketch: {|let update: (Model, Action) -> Model =
  FILL_ME
in
%EXPORT|},
    llm: Azure_GPT4,
    prompt_builder: Filler.prompt(~ctx_init),
  },
  /*
   {
     name: "one",
     sketch: "let lol = FILL_ME in lol + 666",
     llm: Azure_GPT3_5Turbo,
     prompt_builder: Filler.prompt(~ctx_init),
   },
    {
      name: "two",
      sketch: "let lol: Int = FILL_ME in lol + 1337",
      llm: Azure_GPT3_5Turbo,
      prompt_builder: Filler.prompt(~ctx_init),
    },
     {
         name: "three",
         sketch: "let lol: Int-> Bool = FILL_ME in lol(4) && lol(5)",
         llm: Azure_GPT3_5Turbo,
         prompt_builder: Filler.prompt(~ctx_init),
       },*/
];

let is_fill_marker: Piece.t => bool =
  fun
  | Tile({label: [t], _}) => t == fill_marker
  | _ => false;

let mk_script =
    (~llm, ~prompt_builder, ~sketch: string): list(UpdateAction.t) => {
  [
    Reset,
    SwitchScratchSlide(5),
    PerformAction(Move(Extreme(Up))),
    PerformAction(Select(Resize(Extreme(Down)))),
    Paste(sketch),
    PerformAction(Move(Goal(Piece(is_fill_marker, Left)))),
    PerformAction(Select(Term(Current))),
    Paste(Form.expliciter_hole),
    PerformAction(Select(Term(Current))),
    Assistant(Prompt(Filler(Some({llm, prompt_builder})))),
  ];
};

let test_scripts = (~ctx_init: Ctx.t) =>
  List.map(
    ({name, sketch, llm, prompt_builder}) =>
      (name, mk_script(~sketch, ~llm, ~prompt_builder)),
    tests_raw(~ctx_init),
  );

/*
 TODO(andrew):
 - log if we send errors back
 - backup to localstore

 */
