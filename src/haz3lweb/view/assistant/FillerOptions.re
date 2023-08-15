open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  llm: OpenAI.chat_models,
  instructions: bool,
  syntax_notes: bool,
  num_samples: int,
  expected_type: bool,
  error_round: bool,
};

let init: t = {
  llm: Azure_GPT4,
  instructions: true,
  syntax_notes: true,
  num_samples: 9,
  expected_type: true,
  error_round: true,
};
