open Sexplib.Std;
type t = {
  is_demo: bool,
  prompt: option(Prompt.t),
  prompts: list(Prompt.t),
};

let init = {is_demo: true, prompt: None, prompts: Prompt.prompts};

[@deriving sexp]
type update =
  | Set_Demo(bool)
  | Set_Prompt(int);

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Set_Demo(b) => {...settings, is_demo: b}
  | Set_Prompt(p) => {
      ...settings,
      prompt: Some(List.nth(Prompt.prompts, p)),
    }
  };
