open Sexplib.Std;
type t = {
  is_demo: bool,
  prompt: option(Prompt.t),
  prompts: list(Prompt.t),
  example_level: int,
  hovered_over: int,
};

let init = {
  is_demo: true,
  prompt: None,
  prompts: Prompt.prompts,
  example_level: 0,
  hovered_over: (-1),
};

[@deriving sexp]
type update =
  | Set_Demo(bool)
  | Set_Prompt(int)
  | Toggle_Syntactic_Form_Level(int)
  | Toggle_Explanation_Hovered_over(int);

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Set_Demo(b) => {...settings, is_demo: b}
  | Set_Prompt(p) => {
      ...settings,
      prompt: Some(List.nth(Prompt.prompts, p)),
    }
  | Toggle_Syntactic_Form_Level(l) => {...settings, example_level: l}
  | Toggle_Explanation_Hovered_over(l) => {...settings, hovered_over: l}
  };
