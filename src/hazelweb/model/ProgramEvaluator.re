open Lwt.Infix;

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, Lwt.t(ProgramResult.t));
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

  type t = Js.t(Worker.worker(Program.t, ProgramResult.t));

  let init = () => Worker.create("./worker.js");

  let get_result = (worker: t, program: Program.t) => {
    /* FIXME: Starting a new worker everytime seems quite slow. */
    /* Terminate and initialize new worker. */
    print_endline("terminated old worker");
    worker##terminate;

    print_endline("initialized new worker");
    let worker = init();

    let (lwt, resolver) = Lwt.wait();
    worker##.onmessage :=
      Dom.handler(evt => {
        print_endline("received response");

        let r = evt##.data;
        Lwt.wakeup_later(resolver, r);
        Js._true;
      });

    print_endline("posted request");
    worker##postMessage(program);

    (worker, lwt);
  };
};
