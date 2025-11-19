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

module Options = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    params: OpenRouter.params,
    instructions: bool,
    syntax_notes: bool,
    num_examples: int,
    expected_type: bool,
    relevant_ctx: bool,
    error_rounds_max: int,
  };
  let init: t = {
    params: OpenRouter.default_params,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    relevant_ctx: true,
    error_rounds_max: 2,
  };
};

module SystemPrompt = {
  let prelude = ["You are a helpful coding assistant in Hazel. \n"];

  let normal_completion_prompt = (completion_token: string) =>
    CompletionPrompt_normal.self(completion_token);

  let cot_completion_prompt = (completion_token: string) =>
    CompletionPrompt_cot.self(completion_token);

  let hazel_syntax_notes = HazelSyntaxNotes.self;

  let mk_suggestion_prompt =
      (
        {instructions, syntax_notes, _}: Options.t,
        completion_token: string,
        advanced_reasoning: bool,
      )
      : string =>
    String.concat(
      "\n",
      (
        instructions
          ? prelude
            @ (
              advanced_reasoning
                ? cot_completion_prompt(completion_token)
                : normal_completion_prompt(completion_token)
            )
          : []
      )
      @ (syntax_notes ? hazel_syntax_notes : []),
    );
};

let mk_suggestion =
    (hole_label: string, advanced_reasoning: bool): OpenRouter.message => {
  let prompt =
    String.concat(
      "\n",
      [
        SystemPrompt.mk_suggestion_prompt(
          Options.init,
          hole_label,
          advanced_reasoning,
        ),
      ],
      // @ CompletionExamples.get(
      //     Options.init.num_examples,
      //     hole_label,
      //     advanced_reasoning,
      //   ),
    );
  OpenRouter.mk_system_msg(prompt);
};
