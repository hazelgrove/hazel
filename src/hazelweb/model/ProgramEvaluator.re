open Lwt.Infix;

/**
   The type of the evaluation result. [None] indicates some error was encountered.
 */
type program_result = option(ProgramResult.t);

/**
   The type of the deferred evaluation result. See {!type:program_result}.
 */
type deferred_result = Lwt.t(program_result);

module type M = {
  type t;

  let init: unit => t;

  let get_result: (t, Program.t) => (t, deferred_result);
};

module Sync: M = {
  type t = unit;

  let init = () => ();

  let get_result = (t: t, program: Program.t) => {
    let lwt = Lwt.return(program) >|= Program.get_result >|= Option.some;
    (t, lwt);
  };
};

module Worker: M = {
  open Js_of_ocaml;

  type t = {
    /** [worker] is the handle to the active web worker. */
    worker: Js.t(Worker.worker(Program.t, program_result)),
    /** [last] is the last evaluation task. */
    last: option(deferred_result),
  };

  let init = () => {worker: Worker.create("./worker.js"), last: None};

  /**
     [cancel_last t] cancels the last evaluation request and returns [t] with
     [last] set to [None].
   */
  let cancel_last = ({last, _} as t: t) => {
    switch (last) {
    | Some(last) => Lwt.cancel(last)
    | None => ()
    };

    {...t, last: None};
  };

  /**
     [request t program] sends an evaluation request for [program] to the web
     worker.
   */
  let request = ({worker, _}: t, program: Program.t) => {
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

    lwt;
  };

  let get_result = (t: t, program: Program.t) => {
    let t = t |> cancel_last;
    let lwt = program |> request(t);
    ({...t, last: Some(lwt)}, lwt);
  };
};
