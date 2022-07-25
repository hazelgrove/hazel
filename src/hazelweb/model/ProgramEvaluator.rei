/**
  The type of the evaluation result. [EvaluationFail] indicates some error was
  encountered.
 */
[@deriving sexp]
type evaluation_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail;

/**
  The type of the deferred evaluation result. See {!type:evaluation_result}.
 */
type deferred_result = Lwt.t(option(evaluation_result));

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (deferred_result, t);
};

module Sync: M;

module Worker: {
  module Client: M;
  module Worker: WebWorker.WorkerS;
};
