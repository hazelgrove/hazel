/* FIXME: Somehow filter out obsolete results. */
open Lwt.Infix;
open Lwt.Syntax;
open Lwtutil;

[@deriving sexp]
type evaluation_exn =
  | Program_EvalError(EvaluatorError.t)
  | Program_DoesNotElaborate;

[@deriving sexp]
type evaluation_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(evaluation_exn)
  | EvaluationTimeout;

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
    let lwt = {
      let+ r = Lwt.wrap(() => program |> Program.get_result);
      let r =
        switch (r) {
        | r => EvaluationOk(r)
        | exception (Program.EvalError(error)) =>
          EvaluationFail(Program_EvalError(error))
        | exception Program.DoesNotElaborate =>
          EvaluationFail(Program_DoesNotElaborate)
        };

      r;
    };

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

        let on_request = Sync.get_result;
      };
    });

  module Client: M = {
    module Pool = WebWorkerPool.Make(W);
    type t = Pool.t;

    let max = 5;
    let timeout = 2000; /* ms */

    let init = () => {
      let pool = Pool.init(~timeout, ~max);
      let _ = Pool.fill(pool, max);
      pool;
    };

    let get_result = (t: t, program: Program.t) => {
      let res =
        program
        |> Pool.request(t)
        >|= Option.value(~default=EvaluationTimeout);
      (res, t);
    };
  };

  module Worker = W.Worker;
};

module Memoized = (M: M) => {
  include M;

  module Memo = Core_kernel.Memo;
  /* FIXME: Not sure if this just works?? */
  let get_result = Memo.general(~cache_size_bound=5000, get_result);
};

module type STREAMED = {
  type next = Lwt_observable.next(evaluation_result);
  type complete = Lwt_observable.complete;

  type t;
  type subscription;

  let create: unit => (t, Program.t => unit, unit => unit);

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

  let map_program = (inner, program) => {
    let (r, inner') = program |> M.get_result(inner^);
    inner := inner';

    /* No clue why this is necessary but it doesn't work otherwise? */
    let+ r = r;
    r;
  };

  let create = () => {
    let inner = ref(M.init());
    let (observable, next, complete) = Lwt_observable.create();

    let observable =
      observable
      |> Lwt_observable.pipe(
           /* FIXME: Promise failures are lost here, I think? */
           Lwt_stream.map_s(map_program(inner)),
         );

    ({inner, observable}, next, complete);
  };

  let subscribe = ({inner: _, observable}) =>
    Lwt_observable.subscribe(observable);

  let subscribe' = ({inner: _, observable}) =>
    Lwt_observable.subscribe'(observable);

  let wait = ({inner: _, observable}) => Lwt_observable.wait(observable);

  let unsubscribe = Lwt_observable.unsubscribe;
};
