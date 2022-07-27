open Lwtutil;

/**
  The type of an evaluation exception.
 */
[@deriving sexp]
type evaluation_exn =
  | /** Caught {!exception:Program.EvalError}. */
    Program_EvalError(
      EvaluatorError.t,
    )
  | /** Caught {!exception:Program.DoesNotElaborate}. */
    Program_DoesNotElaborate;

/**
  The type of the evaluation result. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type evaluation_result =
  | /** Evaluation succeeded. */
    EvaluationOk(ProgramResult.t)
  | /** Evaluation failed. */
    EvaluationFail(evaluation_exn)
  | /** Evaluation timed out. */
    EvaluationTimeout;

/**
  The type of the deferred evaluation result. See {!type:evaluation_result}.
 */
type deferred_result = Lwt.t(evaluation_result);

/**
  The signature of a program evaluator.
 */
module type M = {
  /**
    The type for a program evaluator.
   */
  type t;

  /**
    [init] is a new program evaluator.
   */
  let init: unit => t;

  /**
    [get_result t program] is [(q, t')], where [t'] contains the new evaluator
    state and [q] is a promise that resolves with an {!type:evaluation_result}.
   */
  let get_result: (t, Program.t) => (deferred_result, t);
};

/**
  Synchronous evaluator which never times out.
 */
module Sync: M;

/**
  Web-worker based evaluator, which uses a pool of workers.
 */
module Worker: {
  /**
    Client for the worker, to be used on the main thread.
   */
  module Client: M;

  /**
    Worker thread code. It should be used like so:
    {[ let () = () |> init |> register ]}.

    Used currently in [Worker.re].
   */
  module Worker: WebWorker.WorkerS;
};

/**
  Functor to create a memoized evaluator.
 */
module Memoized: (M: M) => M;

/**
  Output of the [Streamed] functor. It is a wrapper around
  {!module:Lwt_observable} and should generally be used like one.
 */
module type STREAMED = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  /**
    The type for the evaluator.
   */
  type t;

  /**
    Subscription to the evaluation stream.
   */
  type subscription;

  /**
    [create ()] is (t, next, complete), where [t] is a new program evaluator.
    [next program] asynchronously evaluates [program] and pushes the result to
    the stream. [complete ()] completes the internal stream.
   */
  let create: unit => (t, Program.t => unit, unit => unit);

  /**
    See {!val:Lwt_observable.subscribe}.
   */
  let subscribe: (t, next, complete) => subscription;

  /**
    See {!val:Lwt_observable.subscribe'}.
   */
  let subscribe': (t, next) => subscription;

  /**
    See {!val:Lwt_observable.wait}.
   */
  let wait: t => Lwt.t(unit);

  /**
    See {!val:Lwt_observable.unsubscribe}.
   */
  let unsubscribe: subscription => unit;
};

/**
  Functor to create a streaming evaluator.
 */
module Streamed: (M: M) => STREAMED;
