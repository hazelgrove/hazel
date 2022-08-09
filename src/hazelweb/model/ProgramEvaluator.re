/* See ProgramEvaluator.rei for information. */
open Sexplib.Std;
open Lwt.Infix;
open Lwt.Syntax;
open Lwtutil;

module Id: {
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
type evaluation_request_id = Id.t;

[@deriving sexp]
type evaluation_request = (evaluation_request_id, Program.t);

[@deriving sexp]
type evaluation_exn =
  | Program_EvalError(EvaluatorError.t)
  | Program_DoesNotElaborate;

[@deriving sexp]
type evaluation_result_ =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(evaluation_exn)
  | EvaluationTimeout;

[@deriving sexp]
type evaluation_result = (evaluation_request_id, evaluation_result_);

type deferred_result = Lwt.t(evaluation_result);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, evaluation_request) => (deferred_result, t);
};

module Sync: M = {
  type t = {latest: evaluation_request_id};

  let init = () => {latest: Id.init};

  let get_result = ({latest}: t, (id, program): evaluation_request) => {
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
      type t = evaluation_request;
      type u = string;

      let serialize = program =>
        program |> sexp_of_t |> Sexplib.Sexp.to_string;
      let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
    };

    module Response = {
      [@deriving sexp]
      type t = evaluation_result;
      type u = string;

      let serialize = r => Sexplib.(r |> sexp_of_t |> Sexp.to_string);
      let deserialize = sexp => Sexplib.(sexp |> Sexp.of_string |> t_of_sexp);
    };

    module Worker = {
      /* FIXME: Somehow use constant from dune or something? */
      let file = () => "worker.js";

      type state = Sync.t;

      let init_state = Sync.init;

      let on_request = Sync.get_result;
    };
  });

module Worker: M = {
  type t = W.Client.t;

  let init = W.Client.init;

  let get_result = W.Client.request;
};

module WorkerImpl = W.Worker;

module WorkerPool: M = {
  module Pool = WebWorkerPool.Make(W.Client);
  type t = Pool.t;

  let max = 10;
  let timeout = 2000; /* ms */

  let init = () => {
    let pool = Pool.init(~timeout, ~max);
    let _ = Pool.fill(pool, max);
    pool;
  };

  let get_result = (pool: t, (id, program): evaluation_request) => {
    let res =
      (id, program)
      |> Pool.request(pool)
      >|= Option.value(~default=(id, EvaluationTimeout));
    (res, pool);
  };
};

module Memoized = (M: M) => {
  include M;

  module Memo = Core_kernel.Memo;
  /* FIXME: Not sure if this just works?? */
  let get_result = Memo.general(~cache_size_bound=5000, get_result);
};

module type STREAMED_ = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t;
  type subscription;

  let create: unit => (t, evaluation_request => Lwt.t(unit), unit => unit);

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let unsubscribe: subscription => unit;

  let wait: t => Lwt.t(unit);

  let pipe:
    (Lwt_stream.t(evaluation_result) => Lwt_stream.t('b), t) =>
    Lwt_observable.t('b);
};

module type STREAMED = {
  include STREAMED_;

  module Filtered: STREAMED_;
};

module Streamed = (M: M) => {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t = {
    inner: ref(M.t),
    observable: Lwt_observable.t(evaluation_result),
  };

  type subscription = Lwt_observable.subscription(evaluation_result);

  let map_program = (inner, (id, program)) => {
    let (r, inner') = (id, program) |> M.get_result(inner^);
    inner := inner';

    /* No clue why this is necessary but it doesn't work otherwise? */
    let+ r = r;
    r;
  };

  let create = () => {
    let inner = ref(M.init());
    let (observable, next, complete) = Lwt_observable.create();

    let next = ((id, program)) =>
      Lwt.try_bind(
        () => (id, program) |> map_program(inner),
        r => r |> next |> Lwt.return,
        exn => Lwt.fail(exn),
      );

    ({inner, observable}, next, complete);
  };

  let subscribe = ({inner: _, observable}) =>
    Lwt_observable.subscribe(observable);

  let subscribe' = ({inner: _, observable}) =>
    Lwt_observable.subscribe'(observable);

  let unsubscribe = Lwt_observable.unsubscribe;

  let wait = ({inner: _, observable}) => Lwt_observable.wait(observable);

  let pipe = (f, {inner: _, observable}) =>
    Lwt_observable.pipe(f, observable);

  module Filtered = {
    type nonrec next = next;
    type nonrec complete = complete;

    type nonrec t = {
      inner: t,
      max: ref(evaluation_request_id),
    };

    type nonrec subscription = subscription;

    let create = () => {
      let max = ref(Id.init);

      let ({inner, observable}, next, complete) = create();

      let next = ((id, program)) =>
        if (Id.compare(id, max^) > 0) {
          max := id;
          next((id, program));
        } else {
          Lwt.return_unit;
        };

      /* Filter out obsolete results as they come in. */
      let observable =
        observable
        |> Lwt_observable.pipe(
             Lwt_stream.filter(((id, _)) => Id.compare(id, max^) >= 0),
           );

      let inner = {inner, observable};
      ({inner, max}, next, complete);
    };

    let subscribe = ({inner, _}) => subscribe(inner);
    let subscribe' = ({inner, _}) => subscribe'(inner);
    let unsubscribe = unsubscribe;

    let wait = ({inner, _}) => wait(inner);

    let pipe = (f, {inner, _}) => pipe(f, inner);
  };
};
