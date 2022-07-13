type deferred_result = Lwt.t(ProgramResult.t);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, deferred_result);
};

module Sync: M;
module Worker: M;
