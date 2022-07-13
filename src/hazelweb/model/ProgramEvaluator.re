open Lwt.Infix;

type deferred_result = Lwt.t(ProgramResult.t);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, deferred_result);
};

module Sync: M = {
  type t = unit;

  let init = () => ();

  let get_result = (t: t, program: Program.t) => {
    let lwt = Lwt.return(program) >|= Program.get_result;
    (t, lwt);
  };
};

module Worker: M = {
  open Js_of_ocaml;

  type t = {
    worker: Js.t(Worker.worker(Program.t, ProgramResult.t)),
    last: option(deferred_result),
  };

  let init = () => {worker: Worker.create("./worker.js"), last: None};

  let get_result = ({worker, last}: t, program: Program.t) => {
    /* Cancel last. */
    switch (last) {
    | Some(last) => Lwt.cancel(last)
    | None => ()
    };

    /* Start up new task, resolved when response is received. */
    let (lwt, resolver) = Lwt.task();
    worker##.onmessage :=
      Dom.handler(evt => {
        print_endline("received response");

        let r = evt##.data;
        Lwt.wakeup_later(resolver, r);
        Js._true;
      });

    /* Post evaluation request to worker. */
    print_endline("posted request");
    worker##postMessage(program);

    ({worker, last: Some(lwt)}, lwt);
  };
};
