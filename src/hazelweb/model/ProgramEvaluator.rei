open Lwtutil;

module RequestId: {
  /**
    The type of an evaluation request id.
   */
  [@deriving sexp]
  type t;

  let equal: (t, t) => bool;
  let compare: (t, t) => int;

  /**
    [max id id'] is the id that is larger (and more recent) of the two.
   */
  let max: (t, t) => t;

  /**
    [to_int id] is the integer for the id.
   */
  let to_int: t => int;

  /**
    [init] is the initial id.
   */
  let init: t;

  /**
    [next id] is the next id after [id].
   */
  let next: t => t;
};

/**
  The type of an evaluation request.
 */
[@deriving sexp]
type request = (RequestId.t, Program.t);

/**
  The type of an evaluation exception.
 */
[@deriving sexp]
type exn_error =
  | /** Caught {!exception:Program.EvalError}. */
    Program_EvalError(
      EvaluatorError.t,
    )
  | /** Caught {!exception:Program.DoesNotElaborate}. */
    Program_DoesNotElaborate;

[@deriving sexp]
type response_ =
  | /** Evaluation succeeded. */
    EvaluationOk(ProgramResult.t)
  | /** Evaluation failed. */
    EvaluationFail(exn_error)
  | /** Evaluation timed out. */
    EvaluationTimeout;

/**
  The type of the evaluation response. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type response = (RequestId.t, response_);

/**
  The type of the deferred evaluation response. See {!type:response}.
 */
type deferred_response = Lwt.t(response);

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
    [get_response t program] is [(q, t')], where [t'] contains the new evaluator
    state and [q] is a promise that resolves with an {!type:response}.
   */
  let get_response: (t, request) => (deferred_response, t);
};

/**
  Synchronous evaluator which never times out.
 */
module Sync: M;

/**
  Web worker-based evaluator which uses a worker thread. This is the client
  module, to be used in the main thread.
 */
module Worker: M;

/**
  Web worker thread implementation, to be used in the worker thread.

  It should be used like so:
  {[ let () = () |> init |> register ]}.

  Used currently in [Worker.re].
 */
module WorkerImpl: WebWorker.WorkerS;

/**
  Web-worker based evaluator, which uses a pool of workers.
 */
module WorkerPool: M;

module Memoized: (M: M) => M;

module type STREAMED_ = {
  type next = Lwt_observable.next(response);
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
    [next program] asynchronously evaluates [program] and pushes the response to
    the stream. [complete ()] completes the internal stream.
   */
  let create: unit => (t, request => Lwt.t(unit), unit => unit);

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
    (Lwt_stream.t(response) => Lwt_stream.t('b), t) => Lwt_observable.t('b);
};

/**
  Output of the [Streamed] functor. It is a wrapper around
  {!module:Lwt_observable} and should generally be used like one.

  See {!STREAMED_}.
 */
module type STREAMED = {
  include STREAMED_;

  /**
    An evaluator stream in which obsolute responses (determined by comparison to
    highest seen id value) are filtered out.
   */
  module Filtered: STREAMED_;
};

/**
  Functor to create an evaluator stream.
 */
module Streamed: (M: M) => STREAMED;
