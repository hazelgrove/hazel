open Haz3lcore;
open ExplainThisForm;
open Example;

let (pipe_arg, pipe_fun) = (exp("e_arg"), exp("e_fun"));

let single = (~arg_id: Id.t, ~fn_id: Id.t): single_doc => {
  group_id: PipelineExp,
  form_id: PipelineExp,
  syntactic_form: [pipe_arg, space(), pipeline(), space(), pipe_fun],
  colorings: [(Piece.id(pipe_arg), arg_id), (Piece.id(pipe_fun), fn_id)],
  explanation:
    Printf.sprintf(
      "Passes the [*argument*](%s) to the [*function*](%s).",
      arg_id |> Id.to_string,
      fn_id |> Id.to_string,
    ),
  examples: [
    {
      sub_id: Pipeline1,
      term: mk_example("1 |> fun x -> x + 1"),
      message: {|
              The argument 1 is passed to an increment function, and the entire expression evaluates to 2.
              The pipeline operator is useful for chaining functions together.
              |},
    },
  ],
};
