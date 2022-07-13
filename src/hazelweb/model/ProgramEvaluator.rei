module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, Lwt.t(ProgramResult.t));
};

module Sync: M;
module Worker: M;
