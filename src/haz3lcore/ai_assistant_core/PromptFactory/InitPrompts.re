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
      CompositionPrompt.self
      @ [CompositionPrompt09282025.HazelAgentPrompts.self]
      @ HazelDocumentation.self(~summarized=true),
    ),
  );
};

let mk_suggestion =
    (options: ChatLSP.Options.t, hole_label: string, advanced_reasoning: bool)
    : OpenRouter.message => {
  let prompt =
    String.concat(
      "\n",
      [
        ChatLSP.SystemPrompt.mk_suggestion_prompt(
          options,
          hole_label,
          advanced_reasoning,
        ),
      ]
      @ CompletionExamples.get(
          options.num_examples,
          hole_label,
          advanced_reasoning,
        ),
    );
  OpenRouter.mk_system_msg(prompt);
};
