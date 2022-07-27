module ProgramEvaluator = {
  module Inner = ProgramEvaluator.Streamed(ProgramEvaluator.Worker.Client);

  type t = {
    inner: Inner.t,
    next: Program.t => unit,
    complete: unit => unit,
  };

  let create = () => {
    let (inner, next, complete) = Inner.create();
    {inner, next, complete};
  };

  let next = ({next, _}: t, model: Model.t) =>
    model |> Model.get_program |> next;
  let complete = ({complete, _}: t) => complete();

  let subscribe = ({inner, _}: t) => Inner.subscribe(inner);
  let subscribe' = ({inner, _}: t) => Inner.subscribe(inner);
};

type t = {evaluator: ProgramEvaluator.t};

let init = () => {
  let evaluator = ProgramEvaluator.create();
  {evaluator: evaluator};
};

let evaluator = ({evaluator}: t) => evaluator;

let evaluator_next = state => state |> evaluator |> ProgramEvaluator.next;
let evaluator_subscribe = state =>
  state |> evaluator |> ProgramEvaluator.subscribe;
let evaluator_subscribe' = state =>
  state |> evaluator |> ProgramEvaluator.subscribe';
