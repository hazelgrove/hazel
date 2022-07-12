open Async_kernel;

type request = {program: Program.t};
type response = {result: ProgramResult.t};

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => Deferred.t(ProgramResult.t);
};

module Sync: M;
module Worker: M;

module Make:
  (M: M) =>
   {
    type t = M.t;

    let init: unit => t;

    let get_result: (t, request) => Deferred.t(response);
  };
