module Js = Js_of_ocaml.Js;
module Worker = Js_of_ocaml.Worker;
open Synthesiscomm;

type t =
  Js.t(Worker.worker(Synthesiscore.Types.input, Synthesiscore.Types.output));

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

 See Synthesizer.rei for a description of
 the limitations on fillings' mutation.
 */
/* let fillings = ref(IntMap.empty); */

// Public write once mutable field.
let schedule_action =
  ref(() => failwith("Synthesizer's schedule action wasn't set."));

let callback = (hole_fillings: Synthesiscore.Types.output) => {
  SynthesisTemp.fillings := Shim.synthesizerToHazel(hole_fillings);
  schedule_action^();
};

// Public field
let start = uhexp => run(callback, Shim.hazelToSynthesizer(uhexp));
