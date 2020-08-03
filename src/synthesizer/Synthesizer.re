module Js = Js_of_ocaml.Js;
module Worker = Js_of_ocaml.Worker;
open Synthesiscomm;

type t =
  Js.t(Worker.worker(MessageTypes.to_worker, MessageTypes.from_worker));

//// DEBUG \\//
let log: MessageTypes.from_worker => unit =
  _ => print_endline("log not implemented");
/* data => {
     Js.export("data", data);
     Js.Unsafe.eval_string({|console.log("WorkerHandler.re Log: " + data);|});
   }; */
//\\ DEBUG //\\

let spawn: unit => t = () => Worker.create("Worker.js");
let worker: ref(option(t)) = ref(None);

let stop: unit => unit =
  () =>
    switch (worker^) {
    | None => ()
    | Some(worker) =>
      Js.export("worker", worker);
      Js.Unsafe.eval_string("worker.terminate();");
    };

let run = (callback, message) => {
  stop();
  let w = spawn();
  worker := Some(w);
  Js.export("message", message);
  Js.export("callback", callback);
  Js.export("stop", stop);
  Js.export("worker", w);
  Js.Unsafe.eval_string(
    {|
    //Set up handler side worker interaction
    worker.onmessage = function(e) {
      stop();
      callback(e.data);
    }

    //Start the worker
    worker.postMessage(message);
    |},
  );
};

let callback = (_hole_fillings: Synthesiscomm.MessageTypes.from_worker) =>
  failwith("Callback!");

let start = uhexp => run(callback, Shim.uHExpToExp(uhexp));
