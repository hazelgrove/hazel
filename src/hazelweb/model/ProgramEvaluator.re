open Lwt.Infix;

[@deriving sexp]
type evaluation_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail;

type deferred_result = Lwt.t(option(evaluation_result));

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
        type t = option(evaluation_result);
        type u = string;

        let serialize = r =>
          Sexplib.(
            r
            |> Conv.sexp_of_option(sexp_of_evaluation_result)
            |> Sexp.to_string
          );
        let deserialize = sexp =>
          Sexplib.(
            sexp
            |> Sexp.of_string
            |> Conv.option_of_sexp(evaluation_result_of_sexp)
          );
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
