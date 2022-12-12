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

    let on_request: (state, Request.t) => (Lwt.t(Response.t), state);
  };
};

module type ClientS = {
  module Request: Serializable;
  module Response: Serializable;

  type t;

  let init: unit => t;

  let get_worker: t => Js.t(Worker.worker(Request.u, Response.u));
  let get_last: t => option(Lwt.t(Response.t));

  let request: (t, Request.t) => (Lwt.t(Response.t), t);
  let terminate: t => unit;
};

module type WorkerS = {
  type state;
  type t;

  let init: unit => t;

  let get_state: t => state;

  let register: t => unit;
};

module type S = {
  module Request: Serializable;
  module Response: Serializable;

  module Client:
    ClientS with module Request = Request and module Response = Response;

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

    let request = ({worker, last: _}: t, req: Request.t) => {
      /* Start up new task, resolved when response is received. */
      let (lwt, resolver) = Lwt.task() /* Catch exceptions, particularly cancellation. */;

      Lwt.on_failure(
        lwt,
        fun
        | Lwt.Canceled => ()
        | exn => Lwt.wakeup_later_exn(resolver, exn),
      );

      worker##.onmessage :=
        Dom.handler(evt => {
          let res = evt##.data |> Response.deserialize;
          Lwt.wakeup_later(resolver, res);
          Js._true;
        }) /* Post request to worker. */;

      worker##postMessage(req |> Request.serialize);

      (lwt, {worker, last: Some(lwt)});
    };

    let terminate = ({worker, _}: t) => worker##terminate;
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
      let req = req |> Request.deserialize /* Pass callback. */;

      let (res, state) = req |> M.Worker.on_request(state) /* Send response. */;

      res >|= respond |> ignore /* Return new state. */;

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
