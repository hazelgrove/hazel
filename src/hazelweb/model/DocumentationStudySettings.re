open Sexplib.Std;
type t = {
  is_demo: bool,
  prompt: option(int),
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

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Set_Demo(b) => {...settings, is_demo: b}
  | Set_Prompt(p) => {...settings, prompt: Some(p)}
  | Update_Prompt(prompt_piece, index, rank) =>
    // Function that takes a list of prompts and updates the nth one
    switch (settings.prompt) {
    | None => settings
    | Some(prompt_index) =>
      let current_prompt = List.nth(settings.prompts, prompt_index);
      let new_prompt =
        switch (prompt_piece) {
        | Explanation =>
          Prompt.update_explanation_rank(current_prompt, index, rank)
        | Example => Prompt.update_example_rank(current_prompt, index, rank) // Call Prompt.update... on the current prompt and then call update_nth
        };
      let new_prompts =
        List.mapi(
          (idx, prompt) =>
            if (idx == prompt_index) {
              new_prompt;
            } else {
              prompt;
            },
          settings.prompts,
        );
      {...settings, prompts: new_prompts};
    }
  | Toggle_Syntactic_Form_Level(l) => {...settings, example_level: l}
  | Toggle_Explanation_Hovered_over(l) => {...settings, hovered_over: l}
  };
