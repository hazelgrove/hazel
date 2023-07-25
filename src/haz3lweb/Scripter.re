open Haz3lcore;
open Sexplib.Std;

let sketch_slide = 7;
let fill_marker = "FILL_ME";

[@deriving (show({with_path: false}), sexp, yojson)]
type test = {
  name: string,
  sketch: string,
  llm: OpenAI.chat_models,
  prompt_builder: Editor.t => option(string),
};

let tests_raw = (~ctx_init): list(test) => [
  {
    name: "one",
    sketch: "let lol = FILL_ME in lol + 666",
    llm: GPT3_5Turbo,
    prompt_builder: Filler.prompt(~ctx_init),
  },
  /*
   {
     name: "two",
     sketch: "let lol: Int = FILL_ME in lol + 1337",
     llm: GPT3_5Turbo,
     prompt_builder: Filler.prompt(~ctx_init),
   },
    {
        name: "three",
        sketch: "let lol: Int-> Bool = FILL_ME in lol(4) && lol(5)",
        llm: GPT3_5Turbo,
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
    SwitchScratchSlide(sketch_slide),
    PerformAction(Move(Extreme(Up))),
    PerformAction(Select(Resize(Extreme(Down)))),
    Paste(sketch),
    PerformAction(Move(Goal(Piece(is_fill_marker, Left)))),
    PerformAction(Select(Term(Current))),
    Paste(Form.expliciter_hole),
    PerformAction(Select(Term(Current))),
    Agent(Prompt(Filler(Some({llm, prompt_builder})))),
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
