open Js_of_ocaml;

module type Serializable = {
  type t;
  type u;

  /**
     [serialize v] is a serialized representation of [t].
   */
  let serialize: t => u;

  /**
     [deserialize s] is [v] where [s] is the serialized representation of [v].
   */
  let deserialize: u => t;
};

module type M = {
  module Request: Serializable;
  module Response: Serializable;

  module Worker: {
    /**
       [file ()] is the JavaScript filename for the worker thread.
     */
    let file: unit => string;

    /**
       The type for the worker's state.
     */
    type state;

    /**
       [init_state ()] is a new state.
     */
    let init_state: unit => state;

    /**
       [on_request state req] is [(state, res)] where [res] is the deferred response.
     */
    let on_request: (state, Request.t) => (Lwt.t(Response.t), state);
  };
};

module type ClientS = {
  module Request: Serializable;
  module Response: Serializable;

  type t;

  /**
     [init ()] is a new worker client. Each call spawns a new worker.
   */
  let init: unit => t;

  /**
     [get_worker t] is the worker client.
   */
  let get_worker: t => Js.t(Worker.worker(Request.u, Response.u));

  /**
     [get_last t] is the last request response, if any.
   */
  let get_last: t => option(Lwt.t(Response.t));

  /**
     [request t req] is [(t, res)] where [res] is the deferred response.
   */
  let request: (t, Request.t) => (Lwt.t(Response.t), t);

  /**
    [terminate t] terminates the worker.
   */
  let terminate: t => unit;
};

module type WorkerS = {
  type state;
  type t;

  /**
     [init ()] is a new worker state.
   */
  let init: unit => t;

  /**
     [get_state t] is the inner {!type:state} of [t].
   */
  let get_state: t => state;

  /**
     [register t] registers [t] to listen for requests from the main
     thread.
   */
  let register: t => unit;
};

module type S = {
  module Request: Serializable;
  module Response: Serializable;

  /**
     The module for the main thread.
   */
  module Client:
    ClientS with module Request = Request and module Response = Response;

  /**
     The module for the web worker thread.
   */
  module Worker: WorkerS;
};

module Make:
  (M: M) =>

    S with
      module Request = M.Request and
      module Response = M.Response and
      type Worker.state = M.Worker.state;
