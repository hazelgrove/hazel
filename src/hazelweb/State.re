/**
  Program evaluator state.
 */
module ProgramEvaluator = {
  module InnerW = ProgramEvaluator.WorkerPool;
  module Inner = ProgramEvaluator.Stream(InnerW);

  type t = {
    inner: Inner.t,
    next: ProgramEvaluator.request => Lwt.t(unit),
    complete: Inner.complete,
  };

  let create = () => {
    let (inner, next, complete) = Inner.create(InnerW.init());
    {inner, next, complete};
  };

  let next = ({next, _}: t, model: Model.t) => {
    model |> Model.get_program |> next;
  };
  let complete = ({complete, _}: t) => complete();

  let subscribe = ({inner, _}: t, next) => Inner.subscribe(inner, next);
  let subscribe' = ({inner, _}: t, next) => Inner.subscribe'(inner, next);
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
