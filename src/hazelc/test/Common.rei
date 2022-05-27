module Compile: {
  let compile: string => string;
  let run: string => string;
  let run_no_output: string => string;
};

module Eval: {
  let parse: string => UHExp.t;
  let elab: UHExp.t => DHExp.t;
  let eval: DHExp.t => EvaluatorResult.t;
  let stringify: EvaluatorResult.t => string;
};
