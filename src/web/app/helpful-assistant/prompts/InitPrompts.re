open Util;

let mk_tutor = () => {
  OpenRouter.mk_system_msg(
    String.concat(
      "\n",
      TutorPrompt.self @ HazelDocumentation.self(~summarized=false),
    ),
  );
};

let mk_composition = (): OpenRouter.message => {
  OpenRouter.mk_system_msg(
    String.concat(
      " ",
      CompositionPrompt.self @ HazelDocumentation.self(~summarized=true),
    ),
  );
};

let mk_suggestion = (): OpenRouter.message => {
  OpenRouter.mk_system_msg(
    "You are a helpful assistant that helps the user complete requested completions of holes in the Hazel programming language.",
  );
};
