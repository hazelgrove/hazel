open Async_kernel;
open Js_of_ocaml;
module Memo = Core_kernel.Memo;

type request = {program: Program.t};
type response = {result: ProgramResult.t};

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => Deferred.t(ProgramResult.t);
};

module Sync: M = {
  type t = unit;

  let init = () => ();

  let get_result = (_: t, program: Program.t) =>
    Deferred.create(cell => {
      let r = program |> Program.get_result;
      Ivar.fill(cell, r);
    });
};

module Worker: M = {
  type t = Js.t(Worker.worker(request, response));

  let init = () => Worker.create("worker.js");

  let get_result = (worker: t, program: Program.t) =>
    Deferred.create(cell => {
      worker##.onmessage :=
        Dom.handler(evt => {
          let {result} = evt##.data;
          Ivar.fill(cell, result);
          Js._true;
        });

      worker##postMessage({program: program});
    });
};

module Make = (M: M) => {
  type t = M.t;

  let init = M.init;

  let get_result = (t: t, {program}: request): Deferred.t(response) =>
    program |> M.get_result(t) >>| (result => {result: result});
};
