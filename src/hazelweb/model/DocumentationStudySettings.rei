type t = {
  is_demo: bool,
  prompt: option(int),
  prompts: list(Prompt.t),
  example_level: int,
  hovered_over: int,
};

let init: t;

[@deriving sexp]
type prompt_piece =
  | Explanation
  | Example;

[@deriving sexp]
type update =
  | Set_Demo(bool)
  | Set_Prompt(int)
  | Toggle_Syntactic_Form_Level(int)
  | Toggle_Explanation_Hovered_over(int)
  | Update_Prompt(prompt_piece, int, int);

let apply_update: (update, t) => t;
