open Sexplib.Std;
module Js = Js_of_ocaml.Js;

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

let print_time_and_prompt_to_console = prompts => {
  // type timestamp = {
  //   year: int,
  //   month: int,
  //   day: int,
  //   hours: int,
  //   minutes: int,
  //   seconds: int,
  //   milliseconds: int,
  // };

  let time_: Update.timestamp = Update.get_current_timestamp();
  print_endline(string_of_int(time_.year));
  Prompt.print_to_console(prompts) |> Js.string |> JSUtil.log;
};

[@deriving sexp]
type update =
  | Set_Demo(bool)
  | Set_Prompt(int)
  | Toggle_Syntactic_Form_Level(int)
  | Toggle_Explanation_Hovered_over(int)
  | Update_Prompt(prompt_piece, int, int)
  | Update_Prompt_Text(prompt_piece, string);

let apply_update = (u: update, settings: t) =>
  switch (u) {
  | Set_Demo(b) => {...settings, is_demo: b}
  | Set_Prompt(p) => {...settings, prompt: Some(p)}
  | Update_Prompt(prompt_piece, index, rank) =>
    let temp_prompts = settings.prompts;
    print_time_and_prompt_to_console(temp_prompts);
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
    };

  | Update_Prompt_Text(prompt_piece, text) =>
    // Function that takes a list of prompts and updates the nth one
    switch (settings.prompt) {
    | None => settings
    | Some(prompt_index) =>
      let current_prompt = List.nth(settings.prompts, prompt_index);
      let new_prompt =
        switch (prompt_piece) {
        | Explanation => Prompt.update_explanation_text(current_prompt, text)
        | Example => Prompt.update_example_text(current_prompt, text) // Call Prompt.update... on the current prompt and then call update_nth
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

//TODO fire this
//
