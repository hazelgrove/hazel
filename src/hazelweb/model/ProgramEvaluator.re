open Sexplib.Std;
open Lwt.Syntax;
open Lwtutil;

module RequestId: {
  [@deriving sexp]
  type t;

  let equal: (t, t) => bool;
  let compare: (t, t) => int;
  let max: (t, t) => t;

  let to_int: t => int;

  let init: t;
  let next: t => t;
} = {
  [@deriving sexp]
  type t = int;

  let equal = Int.equal;
  let compare = Int.compare;
  let max = Int.max;

  let to_int = id => id;

  let init = 0;
  let next = id => id + 1;
};

[@deriving sexp]
type request = (RequestId.t, Program.t);

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

  let get_response: (t, request) => (Lwt.t((RequestId.t, response)), t);
};

module Memoized = (M: M) => {
  include M;

  module Memo = Core_kernel.Memo;
  /* FIXME: Not sure if this just works?? */
  let get_response = Memo.general(~cache_size_bound=5000, get_response);
};

module Sync: M with type response = response = {
  [@deriving sexp]
  type nonrec response = response;

  type t = {latest: RequestId.t};

  let init = () => {latest: RequestId.init};

  let get_response = ({latest}: t, (id, program): request) => {
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
      (id, res);
    };

    (lwt, {latest: latest});
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
      type t = (RequestId.t, response);
      type u = string;

      let serialize = r => Sexplib.(r |> sexp_of_t |> Sexp.to_string);
      let deserialize = sexp => Sexplib.(sexp |> Sexp.of_string |> t_of_sexp);
    };

    module Worker = {
      /* FIXME: Somehow use constant from dune or something? */
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
  let timeout = 2000; /* ms */

  let init = () => {
    let pool = Pool.init(~timeout, ~max);
    let _ = Pool.fill(pool, max);
    pool;
  };

  let get_response = (pool: t, (id, program)) => {
    let res = {
      let+ res = (id, program) |> Pool.request(pool);
      switch (res) {
      | Some((id, res)) => (id, Some(res))
      | None => (id, None)
      };
    };
    (res, pool);
  };
};

module type STREAM = {
  type t_;

  [@deriving sexp]
  type response;

  type t;
  type subscription;

  type next = Lwt_observable.next((RequestId.t, response));
  type complete = Lwt_observable.complete;

  let create: t_ => (t, request => Lwt.t(unit), unit => unit);

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let unsubscribe: subscription => unit;

  let wait: t => Lwt.t(unit);

  let pipe:
    (Lwt_stream.t((RequestId.t, response)) => Lwt_stream.t('b), t) =>
    Lwt_observable.t('b);
};

module Stream = (M: M) => {
  type t_ = M.t;

  [@deriving sexp]
  type response = M.response;

  type t = {
    inner: ref(M.t),
    observable: Lwt_observable.t((RequestId.t, response)),
    max: ref(RequestId.t),
  };

  type subscription = Lwt_observable.subscription((RequestId.t, response));

  type next = Lwt_observable.next((RequestId.t, response));
  type complete = Lwt_observable.complete;

  let map_program = (inner, (id, program)) => {
    let (r, inner') = (id, program) |> M.get_response(inner^);
    inner := inner';

    /* No clue why this is necessary but it doesn't work otherwise? */
    let+ r = r;
    r;
  };

  let create = inner => {
    let inner = ref(inner);
    let (observable, next, complete) = Lwt_observable.create();

    let max = ref(RequestId.init);

    /* Filter out obsolete responses as they come in. */
    let observable =
      observable
      |> Lwt_observable.pipe(
           Lwt_stream.filter(((id, _)) => RequestId.compare(id, max^) >= 0),
         );

    let next = ((id, program)) =>
      Lwt.try_bind(
        () => (id, program) |> map_program(inner),
        r => r |> next |> Lwt.return,
        exn => Lwt.fail(exn),
      );

    let next = ((id, program)) =>
      if (RequestId.compare(id, max^) > 0) {
        max := id;
        next((id, program));
      } else {
        Lwt.return_unit;
      };

    ({inner, observable, max}, next, complete);
  };

  let subscribe = ({inner: _, observable, max: _}) =>
    Lwt_observable.subscribe(observable);

  let subscribe' = ({inner: _, observable, max: _}) =>
    Lwt_observable.subscribe'(observable);

  let unsubscribe = Lwt_observable.unsubscribe;

  let wait = ({inner: _, observable, max: _}) =>
    Lwt_observable.wait(observable);

  let pipe = (f, {inner: _, observable, max: _}) =>
    Lwt_observable.pipe(f, observable);
};
