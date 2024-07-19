open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  params: OpenAI.params,
  instructions: bool,
  syntax_notes: bool,
  num_examples: int,
  expected_type: bool,
  error_rounds_max: int,
  relevant_ctx: bool,
  rag: option(string),
};

let init: t = {
  params: OpenAI.default_params,
  instructions: true,
  syntax_notes: true,
  num_examples: 9,
  expected_type: true,
  error_rounds_max: 2,
  relevant_ctx: true,
  rag: None,
};