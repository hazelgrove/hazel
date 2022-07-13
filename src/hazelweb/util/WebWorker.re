open Js_of_ocaml;
open Lwt.Infix;

module type Serializable = {
  type t;
  type u;
  let serialize: t => u;
  let deserialize: u => t;
};

module type M = {
  module Request: Serializable;
  module Response: Serializable;

  module Worker: {
    let file: unit => string;

    type state;
    let init_state: unit => state;

    let on_request: (state, Request.t) => (state, Lwt.t(Response.t));
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
     [cancel_last t] cancels the last request. See {!val:Lwt.cancel}.
   */
  let cancel_last: t => t;

  /**
     [request t req] is [(t, res)] where [res] is the deferred response.
   */
  let request: (t, Request.t) => (t, Lwt.t(Response.t));
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

module Make = (M: M) => {
  module Request = M.Request;
  module Response = M.Response;

  module Client = {
    module Request = Request;
    module Response = Response;

    type t = {
      worker: Js.t(Worker.worker(Request.u, Response.u)),
      last: option(Lwt.t(Response.t)),
    };

    let init = () => {worker: Worker.create(M.Worker.file()), last: None};

    let get_worker = ({worker, _}: t) => worker;
    let get_last = ({last, _}: t) => last;

    let cancel_last = ({last, _} as t: t) => {
      last |> Option.map(Lwt.cancel) |> ignore;
      {...t, last: None};
    };

    let request = ({worker, last: _}: t, req: Request.t) => {
      /* Start up new task, resolved when response is received. */
      let (lwt, resolver) = Lwt.task();

      /* Catch exceptions, particularly cancellation. */
      Lwt.on_failure(
        lwt,
        fun
        | Lwt.Canceled => ()
        /* FIXME: Print error. */
        | _exn => (),
      );

      worker##.onmessage :=
        Dom.handler(evt => {
          let res = evt##.data |> Response.deserialize;
          Lwt.wakeup_later(resolver, res);
          Js._true;
        });

      /* Post request to worker. */
      worker##postMessage(req |> Request.serialize);

      ({worker, last: Some(lwt)}, lwt);
    };
  };

  module Worker = {
    module Request = Request;
    module Response = Response;

    type state = M.Worker.state;
    type t = {state};

    /**
       [respond res] posts the response [res] to the main thread.
     */
    let respond = (res: Response.t) =>
      res |> Response.serialize |> Js_of_ocaml.Worker.post_message;

    let on_request = ({state}: t, req: Request.u) => {
      /* Deserialize request. */
      let req = req |> Request.deserialize;

      /* Pass callback. */
      let (state, res) = req |> M.Worker.on_request(state);

      /* Send response. */
      res >|= respond |> ignore;

      /* Return new state. */
      {state: state};
    };

    let init = () => {state: M.Worker.init_state()};

    let get_state = ({state}: t) => state;

    let register = (t: t) => {
      /* Mutable state. */
      let t = ref(t);
      Js_of_ocaml.Worker.set_onmessage(req => t := on_request(t^, req));
    };
  };
};
