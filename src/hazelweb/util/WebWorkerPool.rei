/**
  Output signature of the functor {!module:WebWorkerPool.Make}.
 */
module type S = {
  module Request: WebWorker.Serializable;
  module Response: WebWorker.Serializable;

  /**
    The type for the pool.
   */
  type t;

  /**
    [init ~timeout ~max] is a worker pool.
   */
  let init: (~timeout: int, ~max: int) => t;

  /**
    [fill pool count] initializes [count] workers into the pool.
   */
  let fill: (t, int) => Lwt.t(unit);

  /**
    [request pool req] performs a request. See {!module:TimedLwtPool}.
   */
  let request: (t, Request.t) => Lwt.t(option(Response.t));
};

/**
  Functor building an implementation of the worker pool given a worker
  implementation.
 */
module Make:
  (W: WebWorker.ClientS) =>
   S with module Request = W.Request and module Response = W.Response;
