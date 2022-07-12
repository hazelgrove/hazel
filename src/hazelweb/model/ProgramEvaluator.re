open Async_kernel;
open Js_of_ocaml;
module Memo = Core_kernel.Memo;

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, Deferred.t(ProgramResult.t));
};

module Sync: M = {
  type t = unit;

  let init = () => ();

  let get_result = (t: t, program: Program.t) => (
    t,
    Deferred.create(cell => {
      let r = program |> Program.get_result;
      Ivar.fill(cell, r);
    }),
  );
};

module Worker: M = {
  type t = Js.t(Worker.worker(Program.t, ProgramResult.t));

  let init = () => Worker.create("worker.js");

  let get_result = (worker: t, program: Program.t) => {
    /* Terminate and initialize new worker. */
    worker##terminate;
    let worker = init();

    (
      worker,
      Deferred.create(cell => {
        worker##.onmessage :=
          Dom.handler(evt => {
            let result = evt##.data;
            Ivar.fill(cell, result);
            Js._true;
          });

        worker##postMessage(program);
      }),
    );
  };
};
