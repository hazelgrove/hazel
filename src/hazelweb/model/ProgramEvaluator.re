open Sexplib.Std;
open Lwt.Syntax;
open Lwtutil;

[@deriving sexp]
type request = Program.t;

[@deriving sexp]
type exn_error =
  | Program_EvalError(EvaluatorError.t)
  | Program_DoesNotElaborate;

[@deriving sexp]
type response =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(exn_error);

module type M = {
  [@deriving sexp]
  type response;

  type t;

  let init: unit => t;

  let get_response: (t, request) => (Lwt.t(response), t);
};

module Sync: M with type response = response = {
  [@deriving sexp]
  type nonrec response = response;

  type t = unit;

  let init = () => ();

  let get_response = ((): t, program: request) => {
    let lwt = {
      let+ r = Lwt.wrap(() => program |> Program.get_result);
      let res =
        switch (r) {
        | r => EvaluationOk(r)
        | exception (Program.EvalError(error)) =>
          EvaluationFail(Program_EvalError(error))
        | exception Program.DoesNotElaborate =>
          EvaluationFail(Program_DoesNotElaborate)
        };
      res;
    };

    (lwt, ());
  };
};

/** Worker impl. */
module W =
  WebWorker.Make({
    module Request = {
      [@deriving sexp]
      type t = request;
      type u = string;

      let serialize = program =>
        program |> sexp_of_t |> Sexplib.Sexp.to_string;
      let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
    };

    module Response = {
      [@deriving sexp]
      type t = response;
      type u = string;

      let serialize = r => Sexplib.(r |> sexp_of_t |> Sexp.to_string);
      let deserialize = sexp => Sexplib.(sexp |> Sexp.of_string |> t_of_sexp);
    };

    module Worker = {
      /* TODO: Somehow use constant from dune or something? */
      let file = () => "worker.js";

      type state = Sync.t;

      let init_state = Sync.init;

      let on_request = Sync.get_response;
    };
  });

module Worker: M with type response = response = {
  [@deriving sexp]
  type nonrec response = response;

  type t = W.Client.t;

  let init = W.Client.init;

  let get_response = W.Client.request;
};

module WorkerImpl = W.Worker;

module WorkerPool: M with type response = option(response) = {
  module Pool = WebWorkerPool.Make(W.Client);

  [@deriving sexp]
  type nonrec response = option(response);

  type t = Pool.t;

  let max = 10;
  let timeout = 2000 /* ms */;

  let init = () => {
    let pool = Pool.init(~timeout, ~max);
    let _ = Pool.fill(pool, max);
    pool;
  };

  let get_response = (pool: t, program) => {
    let res = program |> Pool.request(pool);
    let _ = Pool.add(pool);
    (res, pool);
  };
};

module Memoized = (M: M) : (M with type response = M.response) => {
  [@deriving sexp]
  type response = M.response;

  type t = {
    inner: M.t,
    tbl: Hashtbl.t(request, response),
  };

  let init = () => {inner: M.init(), tbl: Hashtbl.create(5000)};

  let get_response = ({inner, tbl}, program) => {
    switch (Hashtbl.find_opt(tbl, program)) {
    | Some(res) => (Lwt.return(res), {inner, tbl})
    | None =>
      let (res, inner) = M.get_response(inner, program);
      let res =
        res
        |> Lwt.map(res => {
             Hashtbl.replace(tbl, program, res);
             res;
           });

      (res, {inner, tbl});
    };
  };
};

module type STREAM = {
  type t_;

  [@deriving sexp]
  type response;

  type t;
  type subscription;

  type next = Lwt_observable.next(response);
  type complete = Lwt_observable.complete;

  let create: t_ => (t, request => Lwt.t(unit), unit => unit);

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let unsubscribe: subscription => unit;

  let wait: t => Lwt.t(unit);

  let pipe:
    (Lwt_stream.t(response) => Lwt_stream.t('b), t) => Lwt_observable.t('b);
};

module Stream =
       (M: M)
       : (STREAM with type t_ = M.t and type response = M.response) => {
  type t_ = M.t;

  [@deriving sexp]
  type response = M.response;

  type t = {
    inner: ref(M.t),
    observable: Lwt_observable.t((int, response)),
    max: ref(int),
  };

  type subscription = Lwt_observable.subscription((int, response));

  type next = Lwt_observable.next(response);
  type complete = Lwt_observable.complete;

  let map_program = (inner, program) => {
    let (r, inner') = program |> M.get_response(inner^);
    inner := inner' /* No clue why this is necessary but it doesn't work otherwise? */;

    let+ r = r;
    r;
  };

  let create = inner => {
    let inner = ref(inner);
    let (observable, next, complete) = Lwt_observable.create();
    let max = ref(0) /* Filter out obsolete responses as they come in. */;

    let observable =
      observable
      |> Lwt_observable.pipe(
           Lwt_stream.filter(((id, _)) => Int.compare(id, max^) >= 0),
         );

    let next = program => {
      incr(max);

      Lwt.try_bind(
        () => program |> map_program(inner),
        r => (max^, r) |> next |> Lwt.return,
        exn => Lwt.fail(exn),
      );
    };

    ({inner, observable, max}, next, complete);
  };

  let subscribe = ({inner: _, observable, max: _}, next) =>
    Lwt_observable.subscribe(observable, ((_, r)) => next(r));

  let subscribe' = ({inner: _, observable, max: _}, next) =>
    Lwt_observable.subscribe'(observable, ((_, r)) => next(r));

  let unsubscribe = Lwt_observable.unsubscribe;

  let wait = ({inner: _, observable, max: _}) =>
    Lwt_observable.wait(observable);

  let pipe = (f, {inner: _, observable, max: _}) =>
    Lwt_observable.pipe(
      stream => stream |> Lwt_stream.map(((_, r)) => r) |> f,
      observable,
    );
};
