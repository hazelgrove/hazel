[@deriving sexp]
type profile =
  | Test
  | Bench;

module Compile: {
  let compile_grain: (~profile: profile, string) => string;
  let compile: (~profile: profile, string) => string;
  let run: (~profile: profile, string) => string;
};

module Eval: {
  let parse: (~profile: profile, string) => UHExp.t;
  let elab: (~profile: profile, UHExp.t) => DHExp.t;
  let eval: (~profile: profile, DHExp.t) => EvaluatorResult.t;
  let stringify: (~profile: profile, EvaluatorResult.t) => string;
};
