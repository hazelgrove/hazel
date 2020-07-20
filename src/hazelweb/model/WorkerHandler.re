open Types;

module Js = Js_of_ocaml.Js;
module Worker = Js_of_ocaml.Worker;

type t = Js.t(Worker.worker(to_worker, from_worker));

let spawn: unit => t = () => Worker.create("Worker.js");

let worker: ref(option(t)) = ref(None);

let log = data =>
  print_endline("WorkerHandler.re Log: " ++ string_of_int(data));

let run = (callback, message) => {
  Js.export("message", message);
  Js.export("callback", callback);
  Js.Unsafe.eval_string(
    {|
    //Spawn worker
    var worker = new Worker( "Worker.js" );

    //Set up client side worker interaction
    worker.onmessage = function(e) {
      callback(e.data);
    }

    worker.postMessage(message);
    |},
  );
};
