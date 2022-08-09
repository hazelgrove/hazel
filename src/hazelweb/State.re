/**
  Program evaluator state.
 */
module ProgramEvaluator = {
  module Inner' = ProgramEvaluator.Streamed(ProgramEvaluator.WorkerPool);
  module Inner = Inner'.Filtered;

  type t = {
    inner: Inner.t,
    next: ProgramEvaluator.request => Lwt.t(unit),
    complete: Inner.complete,
    count: ref(ProgramEvaluator.RequestId.t),
  };

  let create = () => {
    let (inner, next, complete) = Inner.create();
    {inner, next, complete, count: ref(ProgramEvaluator.RequestId.init)};
  };

  let next = ({next, count, _}: t, model: Model.t) => {
    let id = count^;
    count := ProgramEvaluator.RequestId.next(count^);
    (id, model |> Model.get_program) |> next;
  };
  let complete = ({complete, _}: t) => complete();

  let subscribe = ({inner, _}: t, next) =>
    Inner.subscribe(inner, ((_, r)) => next(r));
  let subscribe' = ({inner, _}: t, next) =>
    Inner.subscribe(inner, ((_, r)) => next(r));
};

type t = {evaluator: ProgramEvaluator.t};

/**
  [init ()] is the initial state.
 */
let init = () => {
  let evaluator = ProgramEvaluator.create();
  {evaluator: evaluator};
};

/**
  [evaluator state] is the program evaluator.
 */
let evaluator = ({evaluator}: t) => evaluator;

/**
  See {!ProgramEvaluator.next}.
 */
let evaluator_next = state => state |> evaluator |> ProgramEvaluator.next;

/**
  See {!ProgramEvaluator.subscribe}.
 */
let evaluator_subscribe = state =>
  state |> evaluator |> ProgramEvaluator.subscribe;

/**
  See {!ProgramEvaluator.subscribe'}.
 */
let evaluator_subscribe' = state =>
  state |> evaluator |> ProgramEvaluator.subscribe';
