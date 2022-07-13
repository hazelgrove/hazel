type program_result = option(ProgramResult.t);
type deferred_result = Lwt.t(program_result);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, deferred_result);
};

module Sync: M;

module Worker: {
  module Client: M;
  module Worker: WebWorker.WorkerS;
};
