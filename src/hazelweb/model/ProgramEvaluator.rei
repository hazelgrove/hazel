open Lwtutil;

/**
  The type of an evaluation exception.
 */
[@deriving sexp]
type evaluation_exn =
  | Program_EvalError(EvaluatorError.t)
  | Program_DoesNotElaborate;

/**
  The type of the evaluation result. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type evaluation_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(evaluation_exn)
  | EvaluationTimeout;

/**
  The type of the deferred evaluation result. See {!type:evaluation_result}.
 */
type deferred_result = Lwt.t(evaluation_result);

module type M = {
  type t;

  let init: unit => t;

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
  module Client: M;
  module Worker: WebWorker.WorkerS;
};

/**
  Functor to create a memoized evaluator.
 */
module Memoized: (M: M) => M;

module type STREAMED = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t;
  type subscription;

  let init: unit => t;

  let next: (t, Program.t) => Lwt.t(unit);
  let complete: t => Lwt.t(unit);

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let wait: t => Lwt.t(unit);

  let unsubscribe: subscription => unit;
};

/**
  Functor to create a streaming evaluator.
 */
module Streamed: (M: M) => STREAMED;
