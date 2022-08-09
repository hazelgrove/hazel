
open Lwtutil;

/**
  The type of an evaluation request.
 */
[@deriving sexp]
type request = Program.t;

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

/**
  The type of the evaluation response. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type response =
  | /** Evaluation succeeded. */
    EvaluationOk(ProgramResult.t)
  | /** Evaluation failed. */
    EvaluationFail(exn_error);

/**
  The signature of a program evaluator.
 */
module type M = {
  /**
    The type of the response.
   */
  [@deriving sexp]
  type response;

  /**
    The type for a program evaluator.
   */
  type t;

  /**
    [init] is a new program evaluator.
   */
  let init: unit => t;

  /**
    [get_response t program] is [(q, t')], where [t'] contains the new
    evaluator state and [q] is a promise that resolves with an
    {!type:response}.
   */
  let get_response: (t, request) => (Lwt.t(response), t);
};

module Memoized: (M: M) => M;

/**
  Synchronous evaluator which never times out.
 */
module Sync: M with type response = response;

/**
  Web worker-based evaluator which uses a worker thread. This is the client
  module, to be used in the main thread.
 */
module Worker: M with type response = response;

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
module WorkerPool: M with type response = option(response);

/**
  Output of the {!Stream} functor. It is a wrapper around {!Lwt_observable} and
  should generally be used like one.
 */
module type STREAM = {
  /**
    The type of the internal evaluator.
   */
  type t_;

  /**
    The type of the response.
   */
  [@deriving sexp]
  type response;

  /**
    The type for the evaluator.
   */
  type t;

  /**
    Subscription to the evaluation stream.
   */
  type subscription;

  type next = Lwt_observable.next(response);
  type complete = Lwt_observable.complete;

  /**
    [create inner], where [inner] is the internal evaluator, is (t, next,
    complete), where [t] is a new program evaluator.

    [next program] asynchronously evaluates [program] and pushes the response
    to the stream. [complete ()] completes the stream.
   */
  let create: t_ => (t, request => Lwt.t(unit), unit => unit);

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
    (Lwt_stream.t((int, response)) => Lwt_stream.t('b), t) =>
    Lwt_observable.t('b);
};

/**
  Functor to create {!STREAM}.
 */
module Stream:
  (M: M) => STREAM with type t_ = M.t and type response = M.response;
