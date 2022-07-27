module ProgramEvaluator = {
  module Inner' = ProgramEvaluator.Streamed(ProgramEvaluator.Worker.Client);
  module Inner = Inner'.Filtered;

  type t = {
    inner: Inner.t,
    next: ProgramEvaluator.evaluation_request => unit,
    complete: Inner.complete,
    count: ref(int),
  };

  let create = () => {
    let (inner, next, complete) = Inner.create();
    {inner, next, complete, count: ref(0)};
  };

  let next = ({next, count, _}: t, model: Model.t) => {
    let id = count^;
    count := count^ + 1;
    (id, model |> Model.get_program) |> next;
  };
  let complete = ({complete, _}: t) => complete();

  let subscribe = ({inner, _}: t, next) =>
    Inner.subscribe(inner, ((_, r)) => next(r));
  let subscribe' = ({inner, _}: t, next) =>
    Inner.subscribe(inner, ((_, r)) => next(r));
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
