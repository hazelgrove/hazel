module ProgramEvaluator = {
  open ProgramEvaluator;

  include Streamed(Worker.Client);
};

type t = {evaluator: ProgramEvaluator.t};

let init = () => {evaluator: ProgramEvaluator.init()};

let subscribe_evaluator = ({evaluator}: t) =>
  ProgramEvaluator.subscribe(evaluator);
let subscribe_evaluator' = ({evaluator}: t) =>
  ProgramEvaluator.subscribe(evaluator);
