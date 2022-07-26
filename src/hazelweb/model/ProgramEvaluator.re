open Sexplib.Std;
open Lwt.Infix;
open Lwt.Syntax;
open Lwtutil;

[@deriving sexp]
type evaluation_result_ =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail;

[@deriving sexp]
type evaluation_result = option(evaluation_result_);

type deferred_result = Lwt.t(evaluation_result);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (deferred_result, t);
};

module Sync: M = {
  type t = unit;

  let init = () => ();

  let get_result = (t: t, program: Program.t) => {
    /* FIXME: Handle exceptions. */
    let lwt =
      Lwt.return(program)
      >|= Program.get_result
      >|= (r => Some(EvaluationOk(r)));
    (lwt, t);
  };
};

module Worker = {
  module W =
    WebWorker.Make({
      module Request = {
        type t = Program.t;
        type u = string;

        let serialize = program =>
          program |> Program.sexp_of_t |> Sexplib.Sexp.to_string;
        let deserialize = sexp =>
          sexp |> Sexplib.Sexp.of_string |> Program.t_of_sexp;
      };

      module Response = {
        type t = evaluation_result;
        type u = string;

        let serialize = r =>
          Sexplib.(r |> sexp_of_evaluation_result |> Sexp.to_string);
        let deserialize = sexp =>
          Sexplib.(sexp |> Sexp.of_string |> evaluation_result_of_sexp);
      };

      module Worker = {
        /* FIXME: Somehow use constant from dune or something? */
        let file = () => "worker.js";

        type state = Sync.t;

        let init_state = Sync.init;

        /* FIXME: Handle exceptions. */
        let on_request = Sync.get_result;
      };
    });

  module Client: M = {
    module Pool = WebWorkerPool.Make(W);
    include Pool;

    let max = 5;

    let init = () => {
      let pool = Pool.init(~timeout=2000, ~max);
      let _ = Pool.fill(pool, max);
      pool;
    };

    let get_result = (t: t, program: Program.t) => {
      let res = program |> request(t) >|= Option.join;
      (res, t);
    };
  };

  module Worker = W.Worker;
};


module type STREAMED = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t;
  type subscription;

  let init: unit => t;

  let next: (t, Program.t) => unit;
  let complete: t => unit;

  let subscribe: (t, next, complete) => subscription;
  let subscribe': (t, next) => subscription;

  let wait: t => Lwt.t(unit);

  let unsubscribe: subscription => unit;
};

module Streamed = (M: M) => {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t = {
    inner: ref(M.t),
    observable: Lwt_observable.t(evaluation_result),
  };
  type subscription = Lwt_observable.subscription(evaluation_result);

  let init = () => {
    inner: ref(M.init()),
    observable: Lwt_observable.create(),
  };

  let next = ({inner, observable}, program) => {
    let _ = {
      /* Compute result and push to stream. */
      let (r, inner') = program |> M.get_result(inner^);
      let+ () = r >|= Lwt_observable.next(observable);

      /* Update inner state. */
      inner := inner';
    };

    ();
  };

  let complete = ({inner: _, observable}) =>
    Lwt_observable.complete(observable);

  let subscribe = ({inner: _, observable}, next, complete) =>
    Lwt_observable.subscribe(observable, next, complete);

  let subscribe' = ({inner: _, observable}, next) =>
    Lwt_observable.subscribe'(observable, next);

  let wait = ({inner: _, observable}) => Lwt_observable.wait(observable);

  let unsubscribe = Lwt_observable.unsubscribe;
};
