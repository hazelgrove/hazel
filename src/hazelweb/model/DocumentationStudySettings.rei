type t = {
  is_demo: bool,
  prompt: option(Prompt.t),
  prompts: list(Prompt.t),
};

let init: t;

[@deriving sexp]
type update =
  | Set_Demo(bool)
  | Set_Prompt(int);

let apply_update: (update, t) => t;
