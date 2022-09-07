/**
  The program evaluator used in hazelweb. For more detailed information on the
  program evaluator itself, see {!ProgramEvaluator}.

  We use the reactive stream-based API, wrapping a pool of web workers. Thus,
  when evaluation requests are sent (via {!StateEvaluator.next}), this is what
  happens, at a high level:

    + An available worker client is acquired from the worker pool; or, the
      pool waits for a worker to become available. See {!WebWorkerPool} for
      more details.
    + The worker client sends a request to the corresponding worker thread. At
      the same time, a timeout is computed.
    + Upon receiving the request, synchronous evaluation is performed in the
      worker thread. If the client timeout completes before evaluation completes,
      the worker thread is killed and a [None] result is returned.
    + If evaluation successfully completed before the timeout, the response is
      returned to the client and the worker is released back into the pool.
    + The response is pushed a reactive stream (see {!ProgramEvaluator.STREAM}).
      Note that obsolete responses are filtered out.
    + In {!Hazel.on_startup} (on application startup), a subscription was made
      to the stream that schedules {!ModelAction.UpdateResult} actions from
      the results as they come in.
    + {!Update.apply_action} performs the necessary UI updates.
 */
module StateEvaluator = {
  module W = ProgramEvaluator.Memoized(ProgramEvaluator.WorkerPool);
  module Inner = ProgramEvaluator.Stream(W);

  /**
    The type of the evaluator.
   */
  type t = {
    /** The actual evaluator. */
    inner: Inner.t,
    next: ProgramEvaluator.request => Lwt.t(unit),
    complete: Inner.complete,
  };

  /**
    [create ()] is a new evaluator.
   */
  let create = () => {
    let (inner, next, complete) = Inner.create(W.init());
    {inner, next, complete};
  };

  /**
    [next evaluator d] makes an evaluation request for d.
   */
  let next = ({next, _}: t, key: string, d: Haz3lcore.DHExp.t) => {
    next((key, d));
  };

  /**
    [complete evaluator] completes the stream.
   */
  let complete = ({complete, _}: t) => complete();

  /**
    See {!ProgramEvaluator.STREAM.subscribe}.
   */
  let subscribe = ({inner, _}: t, next) => Inner.subscribe(inner, next);

  /**
    See {!ProgramEvaluator.STREAM.subscribe'}.
   */
  let subscribe' = ({inner, _}: t, next) => Inner.subscribe'(inner, next);
};

type t = {evaluator: StateEvaluator.t};

/**
  [init ()] is the initial state.
 */
let init = () => {
  let evaluator = StateEvaluator.create();
  {evaluator: evaluator};
};

/**
  [evaluator state] is the program evaluator.
 */
let evaluator = ({evaluator}: t) => evaluator;

/**
  See {!ProgramEvaluator.next}.
 */
let evaluator_next = state => state |> evaluator |> StateEvaluator.next;

/**
  See {!StateEvaluator.subscribe}.
 */
let evaluator_subscribe = state =>
  state |> evaluator |> StateEvaluator.subscribe;

/**
  See {!StateEvaluator.subscribe'}.
 */
let evaluator_subscribe' = state =>
  state |> evaluator |> StateEvaluator.subscribe';
