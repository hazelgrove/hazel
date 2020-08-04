module Js = Js_of_ocaml.Js;
module Worker = Js_of_ocaml.Worker;
module IntMAp = Map.Make(Int);
open Synthesiscomm;

type t =
  Js.t(Worker.worker(MessageTypes.to_worker, MessageTypes.from_worker));

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

/*
 Public read only mutable field.

 This field is mutable, but its mutation is strictly limited.
 It is only mutated by the "callback" function in this file to
 make it reflect the latest synthesis results immidiatly upon
 the synthesis worker's completion, and before the display is
 updated. Anywhere may view its contents but nowhere else may
 edit it.
 */
let fillings = ref(IntMap.empty);

let callback = (hole_fillings: Synthesiscomm.MessageTypes.from_worker) => {
  fillings := IntMap.map(Shim.expToUHExp, hole_fillings);
  print_endline("Repaint!");
};

//Public field
let start = uhexp => run(callback, Shim.uHExpToExp(uhexp));
