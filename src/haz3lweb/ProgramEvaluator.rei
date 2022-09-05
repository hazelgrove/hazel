open Haz3lcore;
open Lwtutil;

/**
  This module is concerned with the evaluation of programs. Though elaboration,
  evaluation, and postprocessing are actually done in {!Program.get_response},
  this module provides several external interfaces.

  The following are the promise-based evaluators, which all have the signature
  {!M} (see its documentation for usage).

    - {b sync} ({!Sync}): Synchronous evaluator which runs evaluation in the
      current thread.
    - {b worker} ({!Worker}): Web worker-based evaluator which runs evaluation
      in a worker thread. {!WorkerImpl} contains the worker thread implementation.
    - {b worker pool} ({!WorkerPool}): Pool of web worker-based evaluators,
      which uses {b worker} internally.
    - {b memoized} ({!Memoized}): Functor to memoize the evaluation result.

  There is also an interface based on reactive streams (in the style of
  {{: https://rxjs.dev/} RxJS}). It is a thin wrapper around {!Lwt_observable},
  which implements reactive streams based on {!Lwt}. See also the documentation
  for {!STREAM} on usage.

    - {b stream} ({!Stream}): Functor which produces an implementation of
      {!STREAM} for a given {!M}.

  {i For information on how this stuff is used in hazelweb, see} {!State}!
 */

// open Lwtutil;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = string;

/**
  The type of an evaluation request.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type request = (key, DHExp.t);

/**
  The type of the evaluation response. [EvaluationFail] indicates some
  (exceptional) error was encountered.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result =
  | /** Evaluation succeeded. */
    EvaluationOk(ProgramResult.t)
  | /** Evaluation failed. */
    EvaluationFail(ProgramEvaluatorError.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type response = (key, eval_result);

/**
  The signature of a promise-based program evaluator.

  It may be used like so:
  {[
    let program = ... in

    (* Initialize the evaluator *)
    let evaluator = M.init () in

    (* Make a request; [res] is a promise that yields the result *)
    let res, evaluator = M.get_response evaluator program in

    (* Do something when the promise resolves *)
    Lwt.bind res
      (function
        | EvaluationOk res -> (* do something with the result *) ...
        | EvaluationFail err -> (* handle the exception *) ... )
  ]}
 */
module type M = {
  /**
    The type of the response.
   */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type response;

  /**
    The type for a program evaluator.
   */
  type t;

  /**
    [init ()] is a new program evaluator.
   */
  let init: unit => t;

  /**
    [get_response t program] is [(q, t')], where [t'] contains the new
    evaluator state and [q] is a promise that resolves with an
    {!type:response}.
   */
  let get_response: (t, request) => (Lwt.t(response), t);
};

/**
  Synchronous evaluator. See above module level documentation.
 */
module Sync: M with type response = response;

/**
  Web worker-based evaluator which uses a worker thread. This is the client
  module, to be used in the main thread. See above module level documentation.
 */
module Worker: M with type response = response;

/**
  Web worker thread implementation, to be used in the worker thread.

  It should be used like so:
  {[
    let () = () |> init |> register
  ]}.

  Used currently in [Worker.re].
 */
module WorkerImpl: WebWorker.WorkerS;

/**
  Web-worker based evaluator, which uses a pool of workers. See above module
  level documentation.
 */
module WorkerPool: M with type response = (key, option(eval_result));

/**
  Memoized evaluator. See above module level documentation.
 */
module Memoized: (M: M) => M with type response = M.response;

/**
  Output of the {!Stream} functor. It is a wrapper around {!Lwtutil.Lwt_observable}
  (see also its documentation) and should generally be used like one. See also
  above module level documentation.

  It may be used like so:
  {[
    module M = ... : M
    module S = Stream(M)

    (* Initialize the evaluator *)
    let evaluator, next, complete = S.init (M.init ()) in

    (* Subscribe to incoming results *)
    let subscription = S.subscribe evaluator
      (fun res -> (* reactively handle result *)) in

    ...

    (* Make evaluation requests *)
    let program = ... in
    let _ = S.next evaluator program in
    let _ = S.next ... in

    (* Later, unsubscribe *)
    S.unsubscribe subscription;

    (* Close the stream *)
    S.complete;
  ]}
 */
module type STREAM = {
  /**
    The type of the internal evaluator.
   */
  type t_;

  /**
    The type of the response.
   */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type response;

  /**
    The type for the evaluator.
   */
  type t;

  /**
    The type for a subscription to the evaluation stream. See
    {!Lwtutil.Lwt_observable.subscription}.
   */
  type subscription;

  /**
    The type for a subscription callback. See {!Lwtutil.Lwt_observable.next}.
   */
  type next = Lwt_observable.next(response);

  /**
    The type for a subscription completion callback. See
    {!Lwtutil.Lwt_observable.complete}.
   */
  type complete = Lwt_observable.complete;

  /**
    [create inner], where [inner] is the internal evaluator, is (t, next,
    complete), where [t] is a new program evaluator.

    [next program] asynchronously evaluates [program] and pushes the response
    to the stream, giving a promise that resolves when it has been pushed.

    [complete ()] completes the stream.
   */
  let create: t_ => (t, request => Lwt.t(unit), unit => unit);

  /**
    See {!Lwtutil.Lwt_observable.subscribe}.
   */
  let subscribe: (t, next, complete) => subscription;

  /**
    See {!Lwtutil.Lwt_observable.subscribe'}.
   */
  let subscribe': (t, next) => subscription;

  /**
    See {!Lwtutil.Lwt_observable.unsubscribe}.
   */
  let unsubscribe: subscription => unit;

  /**
    See {!Lwtutil.Lwt_observable.wait}.
   */
  let wait: t => Lwt.t(unit);

  /**
    See {!Lwtutil.Lwt_observable.pipe}.
   */
  let pipe:
    (Lwt_stream.t(response) => Lwt_stream.t('b), t) => Lwt_observable.t('b);
};

/**
  Functor to create {!STREAM}.
 */
module Stream:
  (M: M) => STREAM with type t_ = M.t and type response = M.response;
