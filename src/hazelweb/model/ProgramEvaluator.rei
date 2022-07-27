open Lwtutil;

/**
  The type of an evaluation request id.
 */
[@deriving sexp]
type evaluation_request_id = int;

/**
  The type of an evaluation request.
 */
[@deriving sexp]
type evaluation_request = (evaluation_request_id, Program.t);

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

[@deriving sexp]
type evaluation_result_ =
  | /** Evaluation succeeded. */
    EvaluationOk(ProgramResult.t)
  | /** Evaluation failed. */
    EvaluationFail(evaluation_exn)
  | /** Evaluation timed out. */
    EvaluationTimeout;

/**
  The type of the evaluation result. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type evaluation_result = (evaluation_request_id, evaluation_result_);

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
  let get_result: (t, evaluation_request) => (deferred_result, t);
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

module Memoized: (M: M) => M;

module type STREAMED_ = {
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
  let create: unit => (t, evaluation_request => unit, unit => unit);

  /**
    See {!val:Lwt_observable.subscribe}.
   */
  let subscribe: (t, next, complete) => subscription;

  /**
    See {!val:Lwt_observable.subscribe'}.
   */
  let subscribe': (t, next) => subscription;

  /**
    See {!val:Lwt_observable.unsubscribe}.
   */
  let unsubscribe: subscription => unit;

  /**
    See {!val:Lwt_observable.wait}.
   */
  let wait: t => Lwt.t(unit);

  /**
    See {!val:Lwt_observable.pipe}.
   */
  let pipe:
    (Lwt_stream.t(evaluation_result) => Lwt_stream.t('b), t) =>
    Lwt_observable.t('b);
};

/**
  Output of the [Streamed] functor. It is a wrapper around
  {!module:Lwt_observable} and should generally be used like one.

  See {!modtype:STREAMED_}.
 */
module type STREAMED = {
  include STREAMED_;

  /**
    An evaluator stream in which obsolute results (determined by comparison to
    highest seen id value) are filtered out.
   */
  module Filtered: STREAMED_;
};

/**
  Functor to create an evaluator stream.
 */
module Streamed: (M: M) => STREAMED;
