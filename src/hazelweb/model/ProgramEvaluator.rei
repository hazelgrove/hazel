open Lwtutil;

[@deriving sexp]
type evaluation_exn =
  | ProgramEvalError(EvaluatorError.t)
  | ProgramDoesNotElaborate;

[@deriving sexp]
type evaluation_result_ =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(evaluation_exn);

/**
  The type of the evaluation result. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type evaluation_result = option(evaluation_result_);

/**
  The type of the deferred evaluation result. See {!type:evaluation_result}.
 */
type deferred_result = Lwt.t(evaluation_result);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (deferred_result, t);
};

module Sync: M;

module Worker: {
  module Client: M;
  module Worker: WebWorker.WorkerS;
};

module Memoized: (M: M) => M;

module type STREAMED = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t;
  type subscription;

  let init: unit => t;

  let next: (t, Program.t) => unit;
  let complete: t => unit;

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let wait: t => Lwt.t(unit);

  let unsubscribe: subscription => unit;
};

module Streamed: (M: M) => STREAMED;
